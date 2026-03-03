module Main where


readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

splitLines :: [String] -> ([String], [String])
splitLines [] = ([], [])
splitLines (x:xs)
  | x == ""   = ([], xs)
  | otherwise =
      let (ranges, ids) = splitLines xs
      in (x : ranges, ids)

convertOne :: String -> (Int, Int)
convertOne str =
  let (a, rest) = break (== '-') str
      b = tail rest
  in (read a, read b)

convertRanges :: [String] -> [(Int, Int)]
convertRanges []     = []
convertRanges (x:xs) = convertOne x : convertRanges xs

sortRanges :: [(Int, Int)] -> [(Int, Int)]
sortRanges [] = []
sortRanges (r:rs) = insertRange r (sortRanges rs)

insertRange :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertRange r [] = [r]
insertRange (a,b) ((c,d):xs)
  | a <= c    = (a,b) : (c,d) : xs
  | otherwise = (c,d) : insertRange (a,b) xs

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges ((a,b):(c,d):xs)
  | c <= b + 1 =
      mergeRanges ((a, max b d) : xs)
  | otherwise =
      (a,b) : mergeRanges ((c,d) : xs)

rangeSize :: (Int, Int) -> Int
rangeSize (a,b) = b - a + 1

sumSizes :: [(Int, Int)] -> Int
sumSizes [] = 0
sumSizes (r:rs) = rangeSize r + sumSizes rs

countFreshIdsFromRanges :: [(Int, Int)] -> Int
countFreshIdsFromRanges ranges =
  let sorted = sortRanges ranges
      merged = mergeRanges sorted
  in sumSizes merged

main :: IO ()
main = do
  ls <- readLines "input.txt"
  let (rangeLines, _idLines) = splitLines ls
      ranges = convertRanges rangeLines
      answer = countFreshIdsFromRanges ranges
  print answer
