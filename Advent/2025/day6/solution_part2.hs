dropTrailingEmptyLines :: [String] -> [String]
dropTrailingEmptyLines xs =
  reverse (dropWhile null (reverse xs))

maximumLen :: [String] -> Int
maximumLen []     = 0
maximumLen (x:xs) = max (length x) (maximumLen xs)

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

slice :: Int -> Int -> String -> String
slice l r s = take (r - l + 1) (drop l s)

columnUsed :: [String] -> Int -> Bool
columnUsed [] _ = False
columnUsed (row:rs) c =
  (row !! c /= ' ') || columnUsed rs c

usedColumns :: [String] -> Int -> [Int]
usedColumns rows width =
  [ c | c <- [0 .. width - 1], columnUsed rows c ]

toRanges :: [Int] -> [(Int, Int)]
toRanges []     = []
toRanges (x:xs) = buildRange x x xs
  where
    buildRange start end [] =
      [(start, end)]
    buildRange start end (y:ys)
      | y == end + 1 = buildRange start y ys
      | otherwise    = (start, end) : buildRange y y ys

extractBlock :: [String] -> Int -> Int -> [String]
extractBlock rows l r =
  map (slice l r) rows


isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'

charToDigit :: Char -> Int
charToDigit ch = fromEnum ch - fromEnum '0'

digitsToInteger :: [Char] -> Integer
digitsToInteger = foldl (\acc d -> acc * 10 + toInteger (charToDigit d)) 0

findOp :: String -> Char
findOp [] = error "No operator found in block bottom row"
findOp (c:cs)
  | c == '+' || c == '*' = c
  | otherwise            = findOp cs

readNumberFromCol :: [String] -> Int -> Integer
readNumberFromCol rows col =
  let digitChars = [ row !! col | row <- init rows, isDigit (row !! col) ]
  in digitsToInteger digitChars

evalBlock :: [String] -> Integer
evalBlock blockLines =
  let -- keep full width; don't trim lines here, we need column positions
      nonEmpty = blockLines
      bottom   = last nonEmpty
      opChar   = findOp bottom
      w        = length bottom

      -- a column counts as a number if it has at least one digit above
      colHasDigit c = any (\row -> isDigit (row !! c)) (init nonEmpty)

      cols     = [ c | c <- [0 .. w - 1], colHasDigit c ]

      -- right-to-left columns: order doesn't matter for +/*,
      -- but we’ll still follow the statement:
      colsRTL  = reverse cols

      nums     = map (readNumberFromCol nonEmpty) colsRTL
  in case opChar of
       '+' -> foldl1 (+) nums
       '*' -> foldl1 (*) nums
       _   -> error "Unknown operator"

main :: IO ()
main = do
  content0 <- readFile "input.txt"
  let content = filter (/= '\r') content0
      ls0     = lines content
      ls      = dropTrailingEmptyLines ls0

      maxLen  = maximumLen ls
      padded  = map (padRight maxLen) ls

      used    = usedColumns padded maxLen
      ranges  = toRanges used

      blocks  = map (\(l,r) -> extractBlock padded l r) ranges
      answers = map evalBlock blocks
      total   = sum answers

  print total
