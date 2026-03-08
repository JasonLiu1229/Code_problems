-- ------------------------------------------------------------
-- readLines :: FilePath -> IO [String]
--
-- Reads a file and returns its contents as a list of lines.
--
-- readFile reads the entire file as a single string.
-- lines splits that string into individual lines.
--
-- <$> is the "fmap" operator. It applies lines to the result
-- inside the IO value returned by readFile.
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
--   1. range lines
--   2. ingredient ID lines
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
  | x == ""   = ([], xs)          -- empty line marks the split
  | otherwise =
      let (ranges, ids) = splitLines xs
      in (x : ranges, ids)       -- add current line to ranges


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
--
-- read converts the numeric strings to integers.
-- ------------------------------------------------------------
convertOne :: String -> (Int, Int)
convertOne str =
  let (a, rest) = break (== '-') str
      b = tail rest                
  in (read a, read b)


-- ------------------------------------------------------------
-- convertRanges :: [String] -> [(Int, Int)]
--
-- Converts a list of range strings into a list of integer ranges.
--
-- Example:
--   ["3-5","10-14"]
--   ->
--   [(3,5),(10,14)]
--
-- Implemented recursively:
--   - base case: empty list
--   - recursive case: convert first element and recurse
-- ------------------------------------------------------------
convertRanges :: [String] -> [(Int, Int)]
convertRanges []     = []
convertRanges (x:xs) = convertOne x : convertRanges xs


-- ------------------------------------------------------------
-- checkRange :: (Int, Int) -> Int -> Bool
--
-- Checks whether an ingredient ID lies within a given range.
--
-- Example:
--   range = (3,5)
--   id = 4
--
--   result = True
--
-- Condition:
--   a <= id <= b
-- ------------------------------------------------------------
checkRange :: (Int, Int) -> Int -> Bool
checkRange (a, b) ingId = a <= ingId && ingId <= b


-- ------------------------------------------------------------
-- checkRanges :: [(Int, Int)] -> Int -> Bool
--
-- Checks whether an ingredient ID belongs to ANY of the ranges.
--
-- Recursively checks each range until:
--   - a matching range is found
--   - or all ranges are exhausted
--
-- Example:
--   ranges = [(3,5),(10,14)]
--   id = 11
--
--   result = True
-- ------------------------------------------------------------
checkRanges :: [(Int, Int)] -> Int -> Bool
checkRanges [] _ = False
checkRanges (r:rs) ingId =
  checkRange r ingId || checkRanges rs ingId


-- ------------------------------------------------------------
-- convertIds :: [String] -> [Int]
--
-- Converts a list of ID strings into integers.
--
-- map read applies read to every element.
--
-- Example:
--   ["4","11","20"]
--   ->
--   [4,11,20]
-- ------------------------------------------------------------
convertIds :: [String] -> [Int]
convertIds = map read


-- ------------------------------------------------------------
-- countFresh :: [(Int, Int)] -> [Int] -> Int
--
-- Counts how many ingredient IDs fall within any valid range.
--
-- Steps:
-- 1. filter keeps only IDs that satisfy checkRanges
-- 2. length counts how many remain
--
-- Example:
--   ranges = [(3,5),(10,14)]
--   ids = [4,6,11,20]
--
--   valid IDs = [4,11]
--   result = 2
-- ------------------------------------------------------------
countFresh :: [(Int, Int)] -> [Int] -> Int
countFresh ranges ids =
  length (filter (\i -> checkRanges ranges i) ids)


-- ------------------------------------------------------------
-- Program entry point.
--
-- Steps:
-- 1. Read all lines from input.txt
-- 2. Split them into range definitions and ingredient IDs
-- 3. Convert ranges to integer pairs
-- 4. Convert IDs to integers
-- 5. Count how many IDs fall within any range
-- 6. Print the result
-- ------------------------------------------------------------
main = do
  ls <- readLines "input.txt"
  let (rangeLines, idLines) = splitLines ls
      ranges = convertRanges rangeLines
      ids    = convertIds idLines
      answer = countFresh ranges ids
  print answer
