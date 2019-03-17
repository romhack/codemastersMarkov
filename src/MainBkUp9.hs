module Main where
import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as Bs
--import Text.Printf (printf)
import Data.Word (Word8)
import Data.Binary.Bits.Get (BitGet, getBool, getWord8, runBitGet)
--import Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
--import Control.Monad (replicateM, liftM2, foldM)


type Color = Word8 --2bit color value
type Row = [Color]
type Tile = [Row]
data Lut = Lut {colMode :: Word8, colA :: Color, colB :: Color, colC :: Color}
  deriving (Show)
type Luts = [Lut] -- 4 colors, each has mode and abc

main :: IO ()
main = do
  input <- Bs.readFile "Bee 52 (U).nes"
  let
    binaryTail = Bs.drop  0x47E input--0x67e9 input --
    getBlock = G.runGetOrFail decompressParser binaryTail
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, compressedSize,  plainBytes) ->  print plainBytes

decompressParser :: G.Get [(Luts, Tile)]
decompressParser = runBitGet parseCommand

parseCommand :: BitGet [(Luts, Tile)]
parseCommand  = do
  size <- getWord8 8
  iterateNTimesM 2 parseBlock ([], [[]])--empty lut and result tiles
    where
      parseBlock :: (Luts, Tile) -> BitGet (Luts, Tile)
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
  if mode == 0
    then return $ Lut mode 0xFF 0xFF 0xFF
    else do --mode 1,2,3 need to parse 'a' color
      aCode <- readGolombCode 2
      let
        remain3Colors = filter (/= color) [0..3]
        a = remain3Colors !! aCode
        remain2Colors = filter (/= a) remain3Colors
      case mode of
        1 -> return $ Lut mode a 0xFF 0xFF
        2 -> do
          bCode <- readGolombCode 1
          return $ Lut mode a (remain2Colors !! bCode) 0xFF
        3 -> return $ Lut mode a (head remain2Colors) (remain2Colors !! 1)
        _ -> error "Mode overflow"


parseTile :: Luts -> Row -> BitGet Tile
parseTile luts = iterateNTimesM 8 parseRow

  where
    parseRow :: Row -> BitGet Row
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
          code <- readGolombCode mode
          case mode of
            0 -> return prevCol
            1 -> return $ [a, prevCol] !! code
            2 -> return $ [prevCol, a, b] !! code
            3 -> return $ [prevCol, a, b, c] !! code
            _ -> error "Mode overflow"

iterateNTimesM :: Monad m => Int -> (a -> m a) -> a -> m [a]
-- apply n times to itself, collecting results. initial value is not outputted
iterateNTimesM 0 _ _ = return []
iterateNTimesM n f a = do
  b <- f a
  as <- iterateNTimesM (n-1) f b
  return (b:as)

readGolombCode :: Word8 -> BitGet Int
--stop reading bitstream either on n-th member,or if True bit found
--and output number of this bit sequence
readGolombCode 0 = return 0
readGolombCode count = sum <$> go count
  where
    go :: Word8 -> BitGet [Int]
    go n = do
      curBit <- getWord8 1
      if  n == 1 -- n reached
        then return [fromIntegral curBit]
        else if curBit == 1 -- 1 terminated not reached n
          then return [0]
          else do -- zeroes are ongoing
            rest <- go (n - 1)
            return $ 1 : rest
