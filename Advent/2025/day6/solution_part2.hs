-- ------------------------------------------------------------
-- readLines :: FilePath -> IO [String]
--
-- Reads a file and returns its contents as a list of lines.
--
-- readFile reads the whole file as a single string.
-- lines splits the string into individual lines.
--
-- <$> (fmap) applies the function 'lines' to the result
-- inside the IO value returned by readFile.
-- ------------------------------------------------------------
readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path


-- ------------------------------------------------------------
-- maximumLen :: [String] -> Int
--
-- Finds the length of the longest string in the list.
--
-- Recursively compares the length of the current string
-- with the maximum length of the remaining strings.
-- ------------------------------------------------------------
maximumLen :: [String] -> Int
maximumLen []     = 0
maximumLen (x:xs) = max (length x) (maximumLen xs)


-- ------------------------------------------------------------
-- padRight :: Int -> String -> String
--
-- Pads a string with spaces on the right so its total
-- length becomes n.
--
-- replicate creates the required number of spaces.
--
-- Example:
--   padRight 6 "abc" -> "abc   "
-- ------------------------------------------------------------
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '


-- ------------------------------------------------------------
-- trim :: String -> String
--
-- Removes leading and trailing spaces from a string.
--
-- Works by:
-- 1. removing leading spaces
-- 2. reversing the string
-- 3. removing spaces again
-- 4. reversing back
--
-- Note: This program avoids trimming block lines during
-- evaluation because column positions must remain fixed.
-- ------------------------------------------------------------
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse


-- ------------------------------------------------------------
-- slice :: Int -> Int -> String -> String
--
-- Extracts a substring from index l to r (inclusive).
--
-- Example:
--   slice 2 4 "abcdef" -> "cde"
--
-- drop l removes the first l characters.
-- take (r-l+1) takes the desired length.
-- ------------------------------------------------------------
slice :: Int -> Int -> String -> String
slice l r s = take (r - l + 1) (drop l s)


-- ------------------------------------------------------------
-- columnUsed :: [String] -> Int -> Bool
--
-- Checks whether column c contains any non-space character.
--
-- If any row has a non-space character in that column,
-- the column is considered used.
-- ------------------------------------------------------------
columnUsed :: [String] -> Int -> Bool
columnUsed [] _ = False
columnUsed (row:rs) c =
  (row !! c /= ' ') || columnUsed rs c


-- ------------------------------------------------------------
-- usedColumns :: [String] -> Int -> [Int]
--
-- Returns all column indices that contain at least one
-- non-space character in the grid.
--
-- Uses list comprehension to test every column.
-- ------------------------------------------------------------
usedColumns :: [String] -> Int -> [Int]
usedColumns rows width =
  [ c | c <- [0 .. width - 1], columnUsed rows c ]


-- ------------------------------------------------------------
-- toRanges :: [Int] -> [(Int, Int)]
--
-- Converts a sorted list of column indices into contiguous ranges.
--
-- Example:
--   [2,3,4,7,8] -> [(2,4),(7,8)]
--
-- Consecutive column indices are grouped together.
-- ------------------------------------------------------------
toRanges :: [Int] -> [(Int, Int)]
toRanges []     = []
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
-- Extracts a block of columns from l to r for every row.
--
-- Each row is sliced using the slice function.
--
-- The result is a smaller grid representing one block.
-- ------------------------------------------------------------
extractBlock :: [String] -> Int -> Int -> [String]
extractBlock rows l r =
  map (slice l r) rows


-- ------------------------------------------------------------
-- isDigit :: Char -> Bool
--
-- Checks whether a character is a digit between '0' and '9'.
-- ------------------------------------------------------------
isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'


-- ------------------------------------------------------------
-- charToDigit :: Char -> Int
--
-- Converts a digit character to its numeric value.
--
-- Example:
--   '7' -> 7
--
-- Uses ASCII values via fromEnum.
-- ------------------------------------------------------------
charToDigit :: Char -> Int
charToDigit ch = fromEnum ch - fromEnum '0'


-- ------------------------------------------------------------
-- digitsToInteger :: [Char] -> Integer
--
-- Converts a list of digit characters into a number.
--
-- Uses foldl to build the number digit by digit.
--
-- Example:
--   "123" -> 123
-- ------------------------------------------------------------
digitsToInteger :: [Char] -> Integer
digitsToInteger = foldl (\acc d -> acc * 10 + toInteger (charToDigit d)) 0


-- ------------------------------------------------------------
-- findOp :: String -> Char
--
-- Finds the operator character ('+' or '*') in the bottom
-- row of a block.
--
-- Scans the string until the operator is found.
-- ------------------------------------------------------------
findOp :: String -> Char
findOp [] = error "No operator found in block bottom row"
findOp (c:cs)
  | c == '+' || c == '*' = c
  | otherwise            = findOp cs


-- ------------------------------------------------------------
-- readNumberFromCol :: [String] -> Int -> Integer
--
-- Reads a vertical number from column 'col'.
--
-- Only rows above the bottom row are considered.
-- All digit characters in that column are collected
-- and converted into a single number.
--
-- Example:
--   column digits: ['1','2','3'] -> 123
-- ------------------------------------------------------------
readNumberFromCol :: [String] -> Int -> Integer
readNumberFromCol rows col =
  let digitChars = [ row !! col | row <- init rows, isDigit (row !! col) ]
  in digitsToInteger digitChars


-- ------------------------------------------------------------
-- evalBlock :: [String] -> Integer
--
-- Evaluates one block of the puzzle.
--
-- Steps:
-- 1. The bottom row contains the operator (+ or *).
-- 2. Identify columns that contain digits above.
-- 3. Each such column forms a vertical number.
-- 4. Apply the operator to all numbers.
--
-- Columns are processed right-to-left, although for + and *
-- the order does not affect the result.
-- ------------------------------------------------------------
evalBlock :: [String] -> Integer
evalBlock blockLines =
  let bottom   = last blockLines
      opChar   = findOp bottom
      w        = length bottom

      -- a column counts as a number if it has at least one digit above
      colHasDigit c = any (\row -> isDigit (row !! c)) (init blockLines)

      cols     = [ c | c <- [0 .. w - 1], colHasDigit c ]

      -- process columns from right to left
      colsRTL  = reverse cols

      nums     = map (readNumberFromCol blockLines) colsRTL
  in case opChar of
       '+' -> foldl1 (+) nums
       '*' -> foldl1 (*) nums
       _   -> error "Unknown operator"


-- ------------------------------------------------------------
-- main :: IO ()
--
-- Program entry point.
--
-- Steps:
-- 1. Read lines from input file.
-- 2. Pad all lines to equal length.
-- 3. Find which columns contain any non-space characters.
-- 4. Convert those columns into contiguous ranges.
-- 5. Extract blocks corresponding to those ranges.
-- 6. Evaluate each block.
-- 7. Sum all results and print the total.
-- ------------------------------------------------------------
main :: IO ()
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
