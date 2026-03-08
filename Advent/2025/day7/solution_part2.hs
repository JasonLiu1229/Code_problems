-- ------------------------------------------------------------
-- Type definitions
-- ------------------------------------------------------------
type Pos       = (Int, Int)
type Grid      = [[Char]]
type BeamStart = (Int, Int)

-- ------------------------------------------------------------
-- Grid helpers
-- ------------------------------------------------------------
gridHeight :: Grid -> Int
gridHeight = length

gridWidth :: Grid -> Int
gridWidth []    = 0
gridWidth (r:_) = length r

-- ------------------------------------------------------------
-- Parsing
-- ------------------------------------------------------------
parseGrid :: String -> Grid
parseGrid = lines

findStart :: Grid -> Int
findStart grid =
  case [ col | (col, ch) <- zip [0..] (head grid), ch == 'S' ] of
    (c:_) -> c
    []    -> error "No S found in row 0"

-- ------------------------------------------------------------
-- Precompute splitter index
--
-- splitterIndex !! col = sorted list of rows with '^' in that column
-- Built once, never touch the grid again after this.
-- ------------------------------------------------------------
buildSplitterIndex :: Grid -> [[Int]]
buildSplitterIndex grid =
  [ [ row | (row, line) <- zip [0..] grid
          , col < length line
          , (line !! col) == '^' ]
  | col <- [0 .. gridWidth grid - 1]
  ]

-- ------------------------------------------------------------
-- Memoisation table
--
-- This is the key concept for beginners:
--
-- A memo table stores answers we have already computed so we
-- never compute the same thing twice.
--
-- We represent it as a sorted list of ((col,row), answer) pairs.
-- When we need the answer for a beam-start, we look it up first.
-- If it is there we reuse it. If not we compute it and store it.
--
-- Why do we need this here?
--   Two different parent beams can both spawn a child at (col=5, row=3).
--   Without memo: we compute timelines(5,3) twice (or millions of times).
--   With memo:    we compute timelines(5,3) once, store the answer,
--                 and just look it up the second time.
-- ------------------------------------------------------------
type Memo = [((Int,Int), Int)]

memoLookup :: (Int,Int) -> Memo -> Maybe Int
memoLookup _ []         = Nothing
memoLookup k ((k2,v):rest)
  | k == k2   = Just v
  | otherwise = memoLookup k rest

memoInsert :: (Int,Int) -> Int -> Memo -> Memo
memoInsert k v []            = [(k,v)]
memoInsert k v ((k2,v2):rest)
  | k == k2   = (k,v) : rest       -- update existing
  | otherwise = (k2,v2) : memoInsert k v rest

-- ------------------------------------------------------------
-- Count timelines 
--
-- We use recursion (DFS).
-- Each call asks: "how many timelines does THIS beam produce?"
--
-- Base case:   no splitter below → 1 timeline (beam exits)
-- Recursive:   hit a splitter    → sum of timelines from each child
--
-- The memo table is threaded through every call as an argument.
-- We pass it in and return the updated version alongside the answer.
-- ------------------------------------------------------------
countTimelines :: [[Int]] -> Int -> Int -> Int -> Int -> Memo -> (Int, Memo)
countTimelines splitterIndex width height col startRow memo =
  case memoLookup (col, startRow) memo of
    Just cached -> -- computed before
      (cached, memo)
    Nothing -> -- first time computing it
      let
        hits = filter (>= startRow) (splitterIndex !! col)
        (result, memo') = case hits of
          -- No splitter: beam exits the bottom → exactly 1 timeline
          [] -> (1, memo)
          -- Splitter at hitRow: spawn two children
          (hitRow : _) ->
            let
              candidates = [(col-1, hitRow+1), (col+1, hitRow+1)]
              children   = filter (
                                    \(c,r) -> 
                                            c >= 0    && 
                                            c < width && 
                                            r < height
                                  )
                                  candidates
            in
              case children of               
                [] -> (0, memo) -- no valid children
                _  -> foldl (\(acc, m) (c,r) ->
                               let (n, m') = countTimelines splitterIndex width height c r m
                               in  (acc + n, m'))
                             (0, memo) -- start value
                             children
        memo'' = memoInsert (col, startRow) result memo'
      in
        (result, memo'')

simulate :: Grid -> BeamStart -> Int
simulate grid (startCol, startRow) =
  let splitterIndex = buildSplitterIndex grid
      width         = gridWidth  grid
      height        = gridHeight grid
      (answer, _)   = countTimelines splitterIndex width height startCol startRow []
  in  answer

solve :: String -> Int
solve input =
  let grid     = parseGrid input
      startCol = findStart grid
  in  simulate grid (startCol, 0)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print (solve input)
