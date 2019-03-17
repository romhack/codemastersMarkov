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
import Data.List (sortBy, elemIndex)
import Data.Function (on)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
import qualified Data.Binary.Put as P
--import Data.Binary.Bits.Put (BitPut, putWord8, runBitPut)
import Control.Monad (when, foldM_)


type Color = Word8 --2bit color value
type Row = [Color] --8 pixels line
type Tile = [Row] --8 rows of pixels forms tile
data Lut = Lut {colMode :: Word8, nextCols :: [Color]}
--each lut has mode and maybe 3 following color

  deriving (Show)
type Luts = [Lut] -- 4 luts for 4 possible colors of pixel

main :: IO ()
main = compress

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
          then do --just unpack tile
            tile1 <- parseTile prevLuts prevRow
            return (prevLuts, tile1)
          else do --update luts
            currentLuts <- parseLuts
            tile2 <- parseTile currentLuts prevRow
            return (currentLuts, tile2)

parseLuts :: BitGet Luts -- update 4 luts in the beginning of blocks
parseLuts =  reverse <$> mapM parseColor [3,2..0] --colors are stored in reverse order
  where
    parseColor color = do
      mode <- getWord8 2
      if mode == 0
        then return $ Lut mode []
        else do --mode 1,2,3 need to parse 'a' color
          aCode <- readGolombCode 2
          let
            remain3Colors = filter (/= color) [0..3]
            a = remain3Colors !! aCode
            remain2Colors = filter (/= a) remain3Colors
          case mode of
            1 -> return $ Lut mode [a]
            2 -> do
              bCode <- readGolombCode 1
              return $ Lut mode [a, remain2Colors !! bCode]
            3 -> return $ Lut mode (a : remain2Colors)
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
          let Lut mode nexts = luts !! fromIntegral prevCol
          code <- readGolombCode mode
          case mode of
            0 -> return prevCol
            1 -> return $ [head nexts, prevCol] !! code
            2 -> return $ (prevCol:nexts) !! code
            3 -> return $ (prevCol:nexts) !! code
            _ -> error "Mode overflow in parse"

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

buildLuts :: [Row] -> Luts
buildLuts noRleRows = map buildLut [0..3] --for each possible color
  where
    pairsList :: [(Color, Color)]
    pairsList = concatMap getNoDupsPairsList noRleRows -- no rle pixels in rows
    getNoDupsPairsList :: (Eq a) => [a] -> [(a,a)] --get all couples combinations, except RLE
    getNoDupsPairsList (x1:x2:xs)
      | x1 == x2  = getNoDupsPairsList (x2 : xs)
      | otherwise = (x1, x2) : getNoDupsPairsList (x2 : xs)
    getNoDupsPairsList _ = []

    pairsCounted :: M.Map (Color, Color) Int
    pairsCounted = M.fromListWith (+) [(x, 1) | x <- pairsList] --get histogram
    colorTables =  M.toList pairsCounted
    sortedTables = map fst $ sortBy (flip compare `on` snd) colorTables --descending by frequencies met
    buildLut number = Lut ((fromIntegral.length) nexts) nexts --filter for appropriate color
      where nexts = [next | (cur, next) <- sortedTables, cur == number]

countLutsCost :: Luts -> Int
countLutsCost luts = sum $ zipWith (curry countLutCost) luts [0..3]
  where
    countLutCost :: (Lut, Color) -> Int
    countLutCost (Lut 0 _, _) = 2 --2 bits for mode
    countLutCost (Lut 2 (colA:_), num) = colACost colA num + 2 + 1 --mode 2 excessive bit
    countLutCost (Lut _ (colA:_), num) = colACost colA num + 2
    countLutCost _ = error "Luts mode-nexts mismatch" --empty nexts found
    colACost 0 _ = 1 --zeros are encoded with 1 bit for cols 1,2,3
    colACost 1 0 = 1 -- 1 for col0 is encoded with 1 bit
    colACost _ _ = 2 -- all other nexts are encoded with 2 bits

countPixelCost :: Luts -> [Color] -> Int
countPixelCost luts (prev: cur : xs)
  | mode == 0   = 0 + rest--filled row check first!
  | prev == cur = 1 + rest--rle pixel
  | mode == 1   = 1 + rest--just A color
  | mode == 2   = 2 + rest
  | otherwise   = if cur == (head.nextCols) lut    then 2 + rest  else 3 + rest --mode 3
  where
    lut = luts !! fromIntegral prev
    mode = colMode lut
    rest = countPixelCost luts (cur : xs)
countPixelCost _ _ = 0

removeAdjDups :: (Eq a) => [a] -> [a] --remove adjacent duplicates in list
removeAdjDups (x1:x2:xs)
   | x1 == x2  =      removeAdjDups (x2 : xs)
   | otherwise = x1 : removeAdjDups (x2 : xs)
removeAdjDups xs  = xs

countBlockCost :: [Tile] -> Int
countBlockCost tiles = countLutsCost luts +
                                tilesCount + rowsCount + 2 * noRleRowsCount +
                                sum (map (countPixelCost luts) noRleRows)
-- first luts cost, then
--1bit for lut/tile flag, 1bit for rle/raw rows flag,
--2bits for first colors of non-rle rows and then actual pixels costs
  where
    rows = concat tiles
    noRleRows = removeAdjDups rows
    rowsCount = length rows
    tilesCount = length tiles
    noRleRowsCount = length noRleRows
    luts = buildLuts noRleRows

serializeBlock :: [Tile] -> BitPut()
serializeBlock tiles = putLuts luts >> putTile luts [] (head tiles)
  where
    luts = buildLuts noRleRows
    noRleRows = removeAdjDups rows
    rows = concat tiles

putGolombCode:: Word8 -> Word8 -> BitPut()
putGolombCode len val = putWord8 putLen putBit
  where --len is 1 based, but index is zero-based, so -1
    (putLen, putBit) = (codes !! fromIntegral (len - 1)) !! fromIntegral val
    codes = [[(1,0), (1,1)], --just a LUT for golomb codes (len, value)
              [(1,1), (2,0), (2,1)],
              [(1,1), (2,1), (3,0), (3,1)]]

putLuts:: Luts -> BitPut ()
putLuts luts = putWord8 1 0 >> mapM_ putLut (reverse (luts `zip` [0..3]))
  where
    putLut :: (Lut, Color) -> BitPut()
    putLut (lut, curCol) = do
      let mode = colMode lut
      putWord8 2 mode
      when (mode /= 0) $ do
        let
          remain3Colors = filter (/= curCol) [0..3]
          nexts = nextCols lut
          colA = head nexts
          colAIdx =  fromIntegral $ fromJust $ colA `elemIndex` remain3Colors
        putGolombCode 2 colAIdx
        when (mode == 2) $ do
          let
            colB = nexts !! 1
            remain2Colors = filter (/= colA) remain3Colors
            colBIdx = fromIntegral $ fromJust $ colB `elemIndex` remain2Colors
          putGolombCode 1 colBIdx

putTile :: Luts -> Row -> Tile -> BitPut ()
putTile luts prev tile = mapM_ putRow $ (prev: tile) `zip` tile
  where
    putRow :: (Row, Row) -> BitPut ()
    putRow (prevRow, curRow) = if curRow == prevRow
      then putWord8 1 1 -- put RLE row flag
      else do
        putWord8 1 0 --unpack new row flag
        let
          firstPixel = head curRow
          Lut mode _ = luts !! fromIntegral firstPixel
        putWord8 2 firstPixel --first pixel of row
        when (mode /= 0) $ --mode 0 needs no other bits put
          mapM_ (putPixel luts) $ curRow `zip` tail curRow

putPixel :: Luts -> (Color, Color) -> BitPut ()
putPixel luts (prev, cur) = do
  let
    Lut mode nexts = luts !! fromIntegral prev
    idx = fromIntegral $ fromJust $ cur `elemIndex` (prev:nexts)
  if cur == prev
    then putWord8 1 1 -- put RLE pixel flag
    else case mode of
      1 -> putWord8 1 0 --just A color single bit
      _ -> putGolombCode mode idx



compress :: IO()
compress = do
  input <- Bs.readFile "decompressed.chr"
  let
    inputTiles = deserializeTiles input
    lastTile = length inputTiles
    nodes :: [LNode String]
    nodes = zip [0..lastTile] $ repeat [] -- no need for labels
    edges:: [LEdge Int] --(from, to, cost)
    -- calculate costs for all possible position-lengths of blocks
    edges = [(start, cur, countBlockCost (selectBlock start cur))
              | start <- [0 .. lastTile - 1], cur <- [start + 1 .. lastTile]]
    selectBlock start end = take (end - start) $ drop start inputTiles
    graph :: Gr String Int
    graph = mkGraph nodes edges
    shortestPath = sp 0 lastTile graph
    --shortestLen = spLength 0 lastTile graph
    shortestPairs = shortestPath `zip` tail shortestPath
    tilesBlocks = uncurry selectBlock (0,6)
    serialized = P.runPut . runBitPut $ serializeBlock  tilesBlocks

    luts = buildLuts noRleRows
    noRleRows = removeAdjDups rows
    rows = concat $ uncurry selectBlock (0,6)

  Bs.writeFile "serialized.bin" serialized
  print $ luts
