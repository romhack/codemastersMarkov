module Main where
import qualified Data.Binary.Get      as G
import qualified Data.ByteString.Lazy as Bs

import Data.Word (Word8)
import Data.Binary.Bits.Get (BitGet, getBool, getWord8, runBitGet)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Data.Bits (testBit)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.List (nub, group, sortBy)
import Data.Function (on)
--import Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
--import Control.Monad (replicateM, liftM2, foldM)


type Color = Word8 --2bit color value
type Row = [Color] --8 pixels line
type Tile = [Row] --8 rows of pixels forms tile
data Lut = Lut {colMode :: Word8, --each lut has mode and maybe 3 following color
                colA :: Maybe Color, colB :: Maybe Color, colC :: Maybe Color}
  deriving (Show)
type Luts = [Lut] -- 4 luts for 4 possible colors of pixel

main :: IO ()
main = undefined

decompress :: IO ()
decompress = do
  input <- Bs.readFile "Bee 52 (U).nes"
  let
    binaryTail = Bs.drop  0x47E input--0x67e9 input --
    getBlock = G.runGetOrFail decompressParser binaryTail
  case getBlock of
    Left (_, _, errStr) -> error errStr
    Right (_, compressedSize, plainTiles) ->  do
      Bs.writeFile "decompressed.chr" $ Bs.concat $ map serializeTile plainTiles
      putStrLn $ printf "Compressed block size was 0x%X" compressedSize

decompressParser :: G.Get [Tile]
decompressParser = do
  size <- G.getWord8
  resultTuples <- runBitGet $ iterateNTimesM size parseBlock ([], [[]])
  --start with empty lut and result tiles
  return $ map snd resultTuples --discard luts in result
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

parseLuts :: BitGet Luts -- update 4 luts in the beginning of blocks
parseLuts =  reverse <$> mapM parseColor [3,2..0] --colors are stored in reverse order
  where
    parseColor color = do
      mode <- getWord8 2
      if mode == 0
        then return $ Lut mode Nothing Nothing Nothing
        else do --mode 1,2,3 need to parse 'a' color
          aCode <- readGolombCode 2
          let
            remain3Colors = filter (/= color) [0..3]
            a = remain3Colors !! aCode
            remain2Colors = filter (/= a) remain3Colors
          case mode of
            1 -> return $ Lut mode (Just a) Nothing Nothing
            2 -> do
              bCode <- readGolombCode 1
              return $ Lut mode (Just a) (Just (remain2Colors !! bCode)) Nothing
            3 -> return $ Lut mode (Just a) (Just (head remain2Colors)) (Just (remain2Colors !! 1))
            _ -> error "Mode overflow"



parseTile :: Luts -> Row -> BitGet Tile --just unpack next tile
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
        parsePixel prevCol  = do --parse next pixel in tile's row
          let
            lut = luts !! fromIntegral prevCol
            mode = colMode lut
            a = fromJust $ colA lut
            b = fromJust $ colB lut
            c = fromJust $ colC lut
          code <- readGolombCode mode
          case mode of
            0 -> return prevCol
            1 -> return $ [a, prevCol] !! code
            2 -> return $ [prevCol, a, b] !! code
            3 -> return $ [prevCol, a, b, c] !! code
            _ -> error "Mode overflow"

iterateNTimesM :: Monad m => Word8 -> (a -> m a) -> a -> m [a]
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

byteToBits :: Word8 -> [Bool]
byteToBits b = map (testBit b) [7,6..0]

toBytes :: [Bool] -> [Word8]
toBytes bs = map bitsToByte $ chunksOf 8 bs
  where bitsToByte  = foldl (\a b -> a*2 + if b then 1 else 0) 0


serializeTile :: Tile -> Bs.ByteString -- convert Colors to 2 bpp planar format
serializeTile rows = Bs.pack . toBytes $ bit0Plane ++ bit1Plane
  where
    pixels = concat rows
    bit0Plane = map (`testBit` 0) pixels
    bit1Plane = map (`testBit` 1) pixels

compress :: IO ()
compress = do
  input <- Bs.readFile "decompressed.chr"
  let
    tiles = deserializeTiles input
  putStrLn $ unlines.map (unlines.map show) $ take 2 tiles

deserializeTiles :: Bs.ByteString -> [Tile]
deserializeTiles bs = map toColors $ chunksOf 16 $ Bs.unpack bs
  where
    toColors :: [Word8] -> Tile
    toColors binTile = chunksOf 8 $ map formColor pixelBits --break on rows
      where
        planesBits = map (fromIntegral.fromEnum) $ concatMap byteToBits binTile
        pixelBits = take 64 planesBits `zip` drop 64 planesBits --bit0Plane ++ bit1Plane
        formColor :: (Word8, Word8) -> Color --combine 2 bits for a color value
        formColor (bit0, bit1) = bit0 + bit1 * 2

pairsList :: [a] -> [(a,a)] --get all couples combinations
pairsList xs = xs `zip` tail xs

histogram :: (Ord a) => [a] -> M.Map a Int
histogram xs = M.fromListWith (+) [(x, 1) | x <- xs]

lutFromList :: [Color] -> Lut
lutFromList cs = Lut mode (head maybeTbl) (maybeTbl !! 1) (maybeTbl !! 2)
  where
    mode = fromIntegral $ length cs
    maybeTbl = map Just cs ++ repeat Nothing

buildLuts :: [Row] -> Lut
buildLuts rows = lut0
  where
    uniqueRowsPixels :: [Color] --no rle Rows
    uniqueRowsPixels = concat $ nub rows
    removedEqGroups :: [Color] -- no rle pixels in row
    removedEqGroups = map head $ group uniqueRowsPixels
    pairsCounted :: M.Map (Color, Color) Int
    pairsCounted = histogram $ pairsList removedEqGroups
    colorTables =  M.toList pairsCounted
    sortedTables = map fst $ sortBy (flip compare `on` snd) colorTables --descending by frequencies met
    color0 = [next | (col, next) <- sortedTables, col == 0]
    lut0 = lutFromList color0
