-- ------------------------------------------------------------
-- readLines :: FilePath -> IO [String]
--
-- Reads a file and returns its contents as a list of lines.
--
-- readFile reads the whole file as one string.
-- lines splits the string into individual lines.
--
-- <$> (fmap) applies lines to the result inside the IO context.
-- ------------------------------------------------------------
readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path


-- ------------------------------------------------------------
-- dropTrailingEmptyLines :: [String] -> [String]
--
-- Removes empty lines at the end of a list of strings.
--
-- Steps:
-- 1. Reverse the list
-- 2. Drop empty strings from the front
-- 3. Reverse again to restore the original order
-- ------------------------------------------------------------
dropTrailingEmptyLines :: [String] -> [String]
dropTrailingEmptyLines xs =
  reverse (dropWhile null (reverse xs))


-- ------------------------------------------------------------
-- maximumLen :: [String] -> Int
--
-- Finds the length of the longest string in the list.
--
-- Implemented recursively by comparing the current
-- string length with the maximum of the remaining list.
-- ------------------------------------------------------------
maximumLen :: [String] -> Int
maximumLen []     = 0
maximumLen (x:xs) = max (length x) (maximumLen xs)


-- ------------------------------------------------------------
-- padRight :: Int -> String -> String
--
-- Pads a string with spaces on the right until its length
-- becomes n.
--
-- replicate creates a list containing the required number
-- of spaces.
--
-- Example:
--   padRight 5 "abc" -> "abc  "
-- ------------------------------------------------------------
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '


-- ------------------------------------------------------------
-- trim :: String -> String
--
-- Removes leading and trailing spaces from a string.
--
-- Works by:
-- 1. removing spaces at the beginning
-- 2. reversing the string
-- 3. removing spaces again
-- 4. reversing back
-- ------------------------------------------------------------
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse


-- ------------------------------------------------------------
-- slice :: Int -> Int -> String -> String
--
-- Extracts a substring from index l to index r (inclusive).
--
-- Example:
--   slice 2 4 "abcdef" -> "cde"
--
-- drop l removes the first l characters
-- take (r-l+1) takes the desired substring length
-- ------------------------------------------------------------
slice :: Int -> Int -> String -> String
slice l r s = take (r - l + 1) (drop l s)


-- ------------------------------------------------------------
-- columnUsed :: [String] -> Int -> Bool
--
-- Checks whether column c contains any non-space character.
--
-- If any row has a non-space character in that column,
-- the column is considered "used".
-- ------------------------------------------------------------
columnUsed :: [String] -> Int -> Bool
columnUsed [] _ = False
columnUsed (row:rs) c =
  (row !! c /= ' ') || columnUsed rs c


-- ------------------------------------------------------------
-- usedColumns :: [String] -> Int -> [Int]
--
-- Returns a list of all column indices that contain
-- at least one non-space character.
--
-- Uses list comprehension to check each column.
-- ------------------------------------------------------------
usedColumns :: [String] -> Int -> [Int]
usedColumns rows width =
  [ c | c <- [0 .. width - 1], columnUsed rows c ]


-- ------------------------------------------------------------
-- toRanges :: [Int] -> [(Int, Int)]
--
-- Converts a list of column indices into contiguous ranges.
--
-- Example:
--   [2,3,4,7,8]
--   ->
--   [(2,4),(7,8)]
--
-- Uses a helper function buildRange to extend ranges
-- when consecutive indices are found.
-- ------------------------------------------------------------
toRanges :: [Int] -> [(Int, Int)]
toRanges [] = []
toRanges (x:xs) = buildRange x x xs
  where
    buildRange start end [] =
      [(start, end)]
    buildRange start end (y:ys)
      | y == end + 1 = buildRange start y ys
      | otherwise    = (start, end) : buildRange y y ys


-- ------------------------------------------------------------
-- extractBlock :: [String] -> Int -> Int -> [String]
--
-- Extracts a rectangular block of columns from the grid.
--
-- For each row, slice is used to extract the substring
-- between columns l and r.
-- ------------------------------------------------------------
extractBlock :: [String] -> Int -> Int -> [String]
extractBlock rows l r =
  map (slice l r) rows


-- ------------------------------------------------------------
-- isDigit :: Char -> Bool
--
-- Checks whether a character is a numeric digit.
--
-- Implemented manually using character comparisons.
-- ------------------------------------------------------------
isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'


-- ------------------------------------------------------------
-- charToDigit :: Char -> Int
--
-- Converts a digit character into its numeric value.
--
-- Example:
--   '5' -> 5
--
-- Uses ASCII codes via fromEnum.
-- ------------------------------------------------------------
charToDigit :: Char -> Int
charToDigit ch = fromEnum ch - fromEnum '0'


-- ------------------------------------------------------------
-- digitsToInteger :: [Char] -> Integer
--
-- Converts a list of digit characters into an Integer.
--
-- Example:
--   "123" -> 123
--
-- Uses foldl to accumulate the number:
--   acc * 10 + digit
-- ------------------------------------------------------------
digitsToInteger :: [Char] -> Integer
digitsToInteger = foldl (\acc d -> acc * 10 + toInteger (charToDigit d)) 0


-- ------------------------------------------------------------
-- findOp :: String -> Char
--
-- Finds the operator (+ or *) in the bottom row of a block.
--
-- Recursively scans the string until the operator is found.
-- ------------------------------------------------------------
findOp :: String -> Char
findOp [] = error "No operator found in block"
findOp (c:cs)
  | c == '+' || c == '*' = c
  | otherwise            = findOp cs


-- ------------------------------------------------------------
-- colHasDigitAbove :: [String] -> Int -> Bool
--
-- Checks whether a column contains a digit in any row
-- above the bottom row.
--
-- init rows removes the last row so we only check
-- the rows above the operator line.
-- ------------------------------------------------------------
colHasDigitAbove :: [String] -> Int -> Bool
colHasDigitAbove rows c = any predicate (init rows)
  where 
    predicate row = isDigit (row !! c)


-- ------------------------------------------------------------
-- readNumberFromCol :: [String] -> Int -> Integer
--
-- Reads all digits from a specific column (above the bottom row)
-- and converts them into a single integer.
--
-- Example column:
--   ['1','2','3']
--   -> 123
-- ------------------------------------------------------------
readNumberFromCol :: [String] -> Int -> Integer
readNumberFromCol rows c =
  let digitChars = [ row !! c | row <- init rows, isDigit (row !! c) ]
  in digitsToInteger digitChars


-- ------------------------------------------------------------
-- evalBlock :: [String] -> Integer
--
-- Evaluates a block of the puzzle.
--
-- Steps:
-- 1. The last row contains the operator.
-- 2. Identify which columns contain digits above the operator.
-- 3. Read numbers from those columns.
-- 4. Apply the operator to the numbers.
--
-- '+'  -> sum the numbers
-- '*'  -> multiply the numbers
-- ------------------------------------------------------------
evalBlock :: [String] -> Integer
evalBlock blockLines =
  let bottom = last blockLines
      opChar = findOp bottom
      w      = length bottom
      cols   = [ c | c <- [0 .. w - 1], colHasDigitAbove blockLines c ]
      nums   = map (readNumberFromCol blockLines) (reverse cols)
  in case opChar of
       '+' -> foldl1 (+) nums
       '*' -> foldl1 (*) nums
       _   -> error "Unknown operator in block"


-- ------------------------------------------------------------
-- main
--
-- Program entry point.
--
-- Steps:
-- 1. Read input lines
-- 2. Pad lines to equal length
-- 3. Determine which columns are used
-- 4. Convert used columns into ranges (blocks)
-- 5. Extract each block
-- 6. Evaluate each block
-- 7. Sum all results
-- ------------------------------------------------------------
main = do
  ls <- readLines "input.txt"
  let maxLen  = maximumLen ls
      padded  = map (padRight maxLen) ls
      used    = usedColumns padded maxLen
      ranges  = toRanges used
      blocks  = map (\(l,r) -> extractBlock padded l r) ranges
      answers = map evalBlock blocks
      total   = sum answers

  print total
