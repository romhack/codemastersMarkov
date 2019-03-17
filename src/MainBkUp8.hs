module Main where
import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as Bs
--import Text.Printf (printf)
import Data.Word (Word8)
import Data.Binary.Bits.Get (BitGet, getBool, getWord8, runBitGet)
--import Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
--import Control.Monad (replicateM, liftM2, foldM)


type Color = Word8 --2bit color value
data Lut = Lut {colMode :: Word8, colA :: Color, colB :: Color, colC :: Color}
  deriving (Show)
type Luts = [Lut] -- 4 colors, each has mode and abc

main :: IO ()
main = do
  input <- Bs.readFile "Bee 52 (U).nes"
  let
    binaryTail = Bs.drop 0x47E input--0x67e9 input --
    getBlock = G.runGetOrFail decompressParser binaryTail
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, compressedSize,  plainBytes) ->  print plainBytes

decompressParser :: G.Get [(Luts, [[Color]])]
decompressParser = runBitGet parseCommand

parseCommand :: BitGet [(Luts, [[Color]])]
parseCommand  = do
  size <- getWord8 8
  iterateNTimesM 2 parseBlock ([], [[]])--empty lut and result tiles
    where
      parseBlock :: (Luts, [[Color]]) -> BitGet (Luts, [[Color]])
      parseBlock (prevLuts, prevTile) = do
        let prevRow = last prevTile
        skipLutsUpdateFlag <- getBool
        if skipLutsUpdateFlag
          then do
            tile1 <- parseTile prevLuts prevRow
            return (prevLuts, tile1)
          else do
            currentLuts <- parseLuts
            tile2 <- parseTile currentLuts prevRow
            return (currentLuts, tile2)

parseLuts :: BitGet Luts
parseLuts =  reverse <$> mapM parseColor [3,2..0] --colors are stored in reverse order

parseColor :: Color -> BitGet Lut
parseColor color = do
  mode <- getWord8 2
  case mode of
    0 -> return $ Lut mode 0xFF 0xFF 0xFF
    _ -> do --mode 1,2,3 need to parse 'a' color
      let
        remain3Colors = filter (/= color) [0..]
      aCode <- readGolombCode 2
      let a = remain3Colors !! (aCode - 1)
      {-let a
            |aCode == [True]          = head remain3Colors
            |aCode == [False, False]  = remain3Colors !! 1
            |aCode == [False, True]   = remain3Colors !! 2
            | otherwise               = error "golomb read failed"
      --a <- parseA remain3Colors
      -}

      let remain2Colors = filter (/= a) remain3Colors
      case mode of
        1 -> return $ Lut mode a 0xFF 0xFF
        2 -> do
          lastFlag <- getBool
          if lastFlag
            then return $ Lut mode a (remain2Colors !! 1) 0xFF
            else return $ Lut mode a (head remain2Colors) 0xFF
        3 -> return $ Lut mode a (head remain2Colors) (remain2Colors !! 1)
        _ -> error "Mode is not 1,2 or 3"

{-
parseA :: [Color] -> BitGet Color
parseA remain3Colors = do
  fstBit <- getBool
  if fstBit
    then return $ head remain3Colors
    else do
      sndBit <- getBool
      if sndBit
        then return (remain3Colors !! 2)
        else return (remain3Colors !! 1)
-}

parseTile :: Luts -> [Color]-> BitGet [[Color]]
parseTile luts prevTileRow = iterateNTimesM 8 parseRow prevTileRow

  where
    parseRow :: [Color] -> BitGet [Color]
    parseRow prevRow = do
      repeatRowFlag <- getBool
      if repeatRowFlag
        then return prevRow
        else do
          firstCol <- getWord8 2
          rest <- iterateNTimesM 7 parsePixel firstCol
          return $ firstCol : rest

      where
        parsePixel :: Color -> BitGet Color
        parsePixel prevCol  = do
          let
            lut = luts !! fromIntegral prevCol
            mode = colMode lut
            a = colA lut
            b = colB lut
            c = colC lut
          case mode of
            0 -> return prevCol
            _ -> do
              repeatPixelFlag <- getBool
              if repeatPixelFlag
                then return  prevCol
                else case mode of --copy from a,b or c
                    1 ->  return  a
                    2 -> do
                      bFlag <- getBool
                      let result = if bFlag then b else a
                      return result
                    3 -> do
                      aFlag <- getBool
                      if aFlag
                        then return  a
                        else do
                          cFlag <- getBool --optimize that with more general function ternary condition
                          let result = if cFlag then c else b
                          return result
                    _ -> error "Mode is not 1,2 or 3"

iterateNTimesM :: Monad m => Int -> (a -> m a) -> a -> m [a]
-- apply n times to itself, collecting results. initial value is not outputted
iterateNTimesM 0 _ _ = return []
iterateNTimesM n f a = do
  b <- f a
  as <- iterateNTimesM (n-1) f b
  return (b:as)

{-
readGolombCode :: Int -> BitGet [Bool]
--stop reading bitstream until either on n-th member,or if True bit found
readGolombCode count = do
  curBit <- getBool
  if curBit || count == 1
    then return [curBit]
    else do
      rest <- readGolombCode (count - 1)
      return $ curBit : rest
      -}

readGolombCode :: Int -> BitGet Int
--stop reading bitstream until either on n-th member,or if True bit found
readGolombCode count = sum <$> go count
  where
    go :: Int -> BitGet [Int]
    go n = do
      curBit <- getWord8 1
      if  curBit == 1 || n == 1
        then return [fromIntegral curBit]
        else do
          rest <- go (n - 1)
          return $ 2 : rest
