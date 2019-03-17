module Main where
import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as Bs
import Text.Printf (printf)
import Data.Word (Word32, Word8)
import Data.Binary.Bits.Get (BitGet, getWord8, runBitGet)
import Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
import Control.Monad (foldM)


data Luts = Luts {mode :: [Word8], a :: [Word8], b :: [Word8], c :: [Word8]}
  deriving (Show)

main :: IO ()
main = do
  input <- Bs.readFile "Bee 52 (U).nes"
  let
    binaryTail = Bs.drop 0x47E input
    getBlock = G.runGetOrFail decompressParser binaryTail
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, position, plainBytes) -> do
      print $ plainBytes

decompressParser :: G.Get Luts--[Word8]
decompressParser = runBitGet (parseCommand (Luts [] [] [] []))

parseCommand:: Luts -> BitGet Luts--[Word8]
parseCommand luts = do
  size <- getWord8 8
  skipLutFlag <- getWord8 1
  if skipLutFlag == 0
    then do
      newLuts <- parseLuts
      return newLuts--parseTile newLuts
    else
      parseTile luts

parseTile = undefined

parseLuts :: BitGet Luts
parseLuts = do
  (mode, a, b, c) <- foldM parseColor ([],[],[],[]) [3,2..0]
  return (Luts mode a b c)

parseColor :: ([Word8], [Word8], [Word8], [Word8]) -> Word8 -> BitGet ([Word8], [Word8], [Word8], [Word8])
parseColor (modeIn, aIn, bIn, cIn) color = do
  mode <- getWord8 2
  case mode of
    0 -> return (mode:modeIn, 0xFF:aIn, 0xFF:bIn,0xFF:cIn)
    1 -> do
      a <- parseA color
      return (mode:modeIn, a:aIn, 0xFF:bIn,0xFF:cIn)
    2 -> do
      a <- parseA color
      lastFlag <- getWord8 1
      let
        lefts = removeItem a $ (removeItem color [0..3])
        b
          | lastFlag == 1 = lefts !! 1
          | otherwise      = head lefts
      return (mode:modeIn, a:aIn, b:bIn,0xFF:cIn)
    3 -> do
      a <- parseA color
      let lefts = removeItem a $ (removeItem color [0..3])
      return (mode:modeIn, a:aIn, (head lefts):bIn,(lefts !! 1):cIn)



parseA :: Word8 -> BitGet Word8
parseA color = do
  let [a1,a2,a3] = removeItem color [0..3]
  fstBit <- getWord8 1
  if fstBit == 1
    then return a1
    else do
      sndBit <- getWord8 1
      if sndBit == 0 then return a2 else return a3

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
