-- ------------------------------------------------------------
-- readLines :: FilePath -> IO [String]
--
-- Reads a file and returns its contents as a list of lines.
--
-- readFile reads the whole file as one string.
-- lines splits the string into separate lines.
--
-- <$> (fmap) applies lines to the value inside the IO context.
--
-- Example file:
--   3-5
--   10-14
--
-- Result:
--   ["3-5","10-14"]
-- ------------------------------------------------------------
readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path


-- ------------------------------------------------------------
-- splitLines :: [String] -> ([String], [String])
--
-- Splits the input lines into two parts:
--   1. range definitions
--   2. ingredient IDs
--
-- The two sections are separated by an empty line "".
--
-- Example input:
--   ["3-5","10-14","","4","11","20"]
--
-- Output:
--   (["3-5","10-14"],["4","11","20"])
-- ------------------------------------------------------------
splitLines :: [String] -> ([String], [String])
splitLines [] = ([], [])
splitLines (x:xs)
  | x == ""   = ([], xs)     -- empty line marks the split
  | otherwise =
      let (ranges, ids) = splitLines xs
      in (x : ranges, ids)


-- ------------------------------------------------------------
-- convertOne :: String -> (Int, Int)
--
-- Converts a range string like "3-5" into a tuple (3,5).
--
-- break (== '-') splits the string at the first '-'.
--
-- Example:
--   "3-5"
--   break -> ("3","-5")
--
-- tail removes the '-' so we get "5".
-- read converts strings to integers.
-- ------------------------------------------------------------
convertOne :: String -> (Int, Int)
convertOne str =
  let (a, rest) = break (== '-') str
      b = tail rest
  in (read a, read b)


-- ------------------------------------------------------------
-- convertRanges :: [String] -> [(Int, Int)]
--
-- Converts a list of range strings into integer ranges.
--
-- Example:
--   ["3-5","10-14"]
--   ->
--   [(3,5),(10,14)]
--
-- Implemented recursively.
-- ------------------------------------------------------------
convertRanges :: [String] -> [(Int, Int)]
convertRanges []     = []
convertRanges (x:xs) = convertOne x : convertRanges xs


-- ------------------------------------------------------------
-- sortRanges :: [(Int, Int)] -> [(Int, Int)]
--
-- Sorts the ranges by their starting value using insertion sort.
--
-- Algorithm:
-- 1. Recursively sort the tail
-- 2. Insert the head into the correct position
--
-- Example:
--   [(10,14),(3,5)]
--   ->
--   [(3,5),(10,14)]
-- ------------------------------------------------------------
sortRanges :: [(Int, Int)] -> [(Int, Int)]
sortRanges [] = []
sortRanges (r:rs) = insertRange r (sortRanges rs)


-- ------------------------------------------------------------
-- insertRange :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
--
-- Inserts a range into a sorted list of ranges while keeping
-- the list sorted.
--
-- Comparison is done using the start value of the range.
--
-- Example:
--   insertRange (3,5) [(10,14),(16,20)]
--   ->
--   [(3,5),(10,14),(16,20)]
-- ------------------------------------------------------------
insertRange :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertRange r [] = [r]
insertRange (a,b) ((c,d):xs)
  | a <= c    = (a,b) : (c,d) : xs
  | otherwise = (c,d) : insertRange (a,b) xs


-- ------------------------------------------------------------
-- mergeRanges :: [(Int, Int)] -> [(Int, Int)]
--
-- Merges overlapping or adjacent ranges.
--
-- Example:
--   [(3,5),(4,10)]  ->  [(3,10)]
--
-- Adjacent ranges are also merged:
--   [(3,5),(6,8)]   ->  [(3,8)]
--
-- Condition:
--   c <= b + 1
-- means the next range overlaps or touches the current one.
--
-- If they overlap:
--   create a new merged range (a, max b d)
-- ------------------------------------------------------------
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges ((a,b):(c,d):xs)
  | c <= b + 1 =
      mergeRanges ((a, max b d) : xs)
  | otherwise =
      (a,b) : mergeRanges ((c,d) : xs)


-- ------------------------------------------------------------
-- rangeSize :: (Int, Int) -> Int
--
-- Computes how many IDs are contained in a range.
--
-- Formula:
--   size = b - a + 1
--
-- Example:
--   (3,5) -> 3 values: 3,4,5
-- ------------------------------------------------------------
rangeSize :: (Int, Int) -> Int
rangeSize (a,b) = b - a + 1


-- ------------------------------------------------------------
-- sumSizes :: [(Int, Int)] -> Int
--
-- Computes the total number of IDs covered by a list of ranges.
--
-- Example:
--   [(3,5),(10,12)]
--
-- Sizes:
--   (3,5)  -> 3
--   (10,12)-> 3
--
-- Result:
--   6
-- ------------------------------------------------------------
sumSizes :: [(Int, Int)] -> Int
sumSizes [] = 0
sumSizes (r:rs) = rangeSize r + sumSizes rs


-- ------------------------------------------------------------
-- countFreshIdsFromRanges :: [(Int, Int)] -> Int
--
-- Computes how many unique ingredient IDs are covered by the
-- given ranges.
--
-- Steps:
-- 1. Sort ranges
-- 2. Merge overlapping ranges
-- 3. Sum the sizes of the merged ranges
--
-- This avoids double-counting overlapping ranges.
--
-- Example:
--   [(3,5),(4,10)]
--
-- Sorted:
--   [(3,5),(4,10)]
--
-- Merged:
--   [(3,10)]
--
-- Size:
--   8
-- ------------------------------------------------------------
countFreshIdsFromRanges :: [(Int, Int)] -> Int
countFreshIdsFromRanges ranges =
  let sorted = sortRanges ranges
      merged = mergeRanges sorted
  in sumSizes merged


-- ------------------------------------------------------------
-- main
--
-- Program entry point.
--
-- Steps:
-- 1. Read lines from input file
-- 2. Split into range section and ID section
-- 3. Convert range strings into numeric ranges
-- 4. Compute how many IDs are covered by those ranges
-- 5. Print the result
-- ------------------------------------------------------------
main = do
  ls <- readLines "input.txt"
  let (rangeLines, _idLines) = splitLines ls
      ranges = convertRanges rangeLines
      answer = countFreshIdsFromRanges ranges
  print answer
