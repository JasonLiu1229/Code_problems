-- ------------------------------------------------------------
-- Type definitions
--
-- Pos       : position in the grid represented as (column, row)
-- Grid      : the grid itself (list of rows, each row is a list of chars)
-- BeamStart : starting position of a beam (column, startRow)
-- OrdSet a  : simple ordered set implemented as a sorted list
-- ------------------------------------------------------------
type Pos       = (Int, Int)   -- (col, row), 0-indexed
type Grid      = [[Char]]     -- grid.(row).(col) — plain list of lines
type BeamStart = (Int, Int)   -- (col, startRow)
type OrdSet a = [a]


-- Empty set
setEmpty :: OrdSet a
setEmpty = []

-- Insert element into ordered set
-- Keeps the list sorted and avoids duplicates
setInsert :: Ord a => a -> OrdSet a -> OrdSet a
setInsert x []     = [x]
setInsert x (y:ys)
  | x < y    = x : y : ys
  | x == y   = y : ys         -- already present
  | otherwise = y : setInsert x ys

-- Check if element exists in the set
setMember :: Ord a => a -> OrdSet a -> Bool
setMember _ []     = False
setMember x (y:ys)
  | x < y    = False         -- since list is sorted, no need to search further
  | x == y   = True
  | otherwise = setMember x ys

-- Size of set
setSize :: OrdSet a -> Int
setSize = length

-- Create a set containing one element
setSingleton :: a -> OrdSet a
setSingleton x = [x]

-- Convert a list to a set (removes duplicates)
setFromList :: Ord a => [a] -> OrdSet a
setFromList = foldr setInsert setEmpty


-- Number of rows in grid
gridHeight :: Grid -> Int
gridHeight = length

-- Number of columns in grid
gridWidth :: Grid -> Int
gridWidth []    = 0
gridWidth (r:_) = length r

-- Get character at a position if it is inside the grid
-- Returns Nothing if outside bounds
cellAt :: Grid -> Pos -> Maybe Char
cellAt grid (col, row)
  | row < 0 || row >= gridHeight grid = Nothing
  | col < 0 || col >= gridWidth  grid = Nothing
  | otherwise                         = Just ((grid !! row) !! col)


-- ------------------------------------------------------------
-- Parse input into grid
--
-- lines splits input text into rows
-- ------------------------------------------------------------
parseGrid :: String -> Grid
parseGrid = lines   


-- ------------------------------------------------------------
-- Find the column where the start marker 'S' appears
-- in the first row of the grid.
--
-- Returns the first column containing 'S'.
-- ------------------------------------------------------------
findStart :: Grid -> Int
findStart grid =
  case [ col | (col, ch) <- zip [0..] (head grid), ch == 'S' ] of
    (c:_) -> c
    []    -> error "No S found in row 0"


-- ------------------------------------------------------------
-- Simulate beam propagation
--
-- The beam starts at a column and moves downward.
-- If it hits a splitter '^', it splits into two beams:
--
--     left  -> (col - 1, row + 1)
--     right -> (col + 1, row + 1)
--
-- The function counts how many splitters are hit.
-- ------------------------------------------------------------
simulate :: Grid -> BeamStart -> Int
simulate grid start =
    setSize $ go (setSingleton start) setEmpty [start]
  where
    height = gridHeight grid
    maxCol = gridWidth grid - 1

    -- --------------------------------------------------------
    -- Find the first splitter '^' in a column starting
    -- from startRow downward.
    -- --------------------------------------------------------
    firstSplitterRow :: Int -> Int -> Maybe Int
    firstSplitterRow col startRow =
      case filter (\row -> cellAt grid (col, row) == Just '^')
                  [startRow .. height - 1] of
        (r:_) -> Just r
        []    -> Nothing


    -- --------------------------------------------------------
    -- Recursive simulation
    --
    -- visited   : beam start positions already processed
    -- splitters : positions of splitters hit so far
    -- queue     : beams still to simulate
    -- --------------------------------------------------------
    go :: OrdSet BeamStart -> OrdSet Pos -> [BeamStart] -> OrdSet Pos
    go _       splitters []                       = splitters
    go visited splitters ((col, startRow) : rest) =
      case firstSplitterRow col startRow of

        -- no splitter found → beam exits grid
        Nothing ->
          go visited splitters rest

        -- splitter found
        Just hitRow ->
          let
            -- record splitter location
            splitters' = setInsert (col, hitRow) splitters

            -- new beams after split
            children   = [(col - 1, hitRow + 1), (col + 1, hitRow + 1)]

            -- keep only valid beams that:
            -- 1. stay inside grid
            -- 2. have not been visited before
            newChildren =
              filter (\(c, r) ->  c >= 0
                               && c <= maxCol
                               && r < height
                               && not (setMember (c, r) visited))
                     children

            -- mark new beams as visited
            visited' = foldr setInsert visited newChildren

          in
            go visited' splitters' (newChildren ++ rest)


-- ------------------------------------------------------------
-- Solve the problem
--
-- Steps:
-- 1. Parse the grid
-- 2. Find starting column
-- 3. Run simulation from (startCol, 0)
-- ------------------------------------------------------------
solve :: String -> Int
solve input =
  let grid     = parseGrid input
      startCol = findStart grid
  in  simulate grid (startCol, 0)


-- ------------------------------------------------------------
-- Main program
--
-- Reads input file and prints number of splits.
-- ------------------------------------------------------------
main :: IO ()
main = do
  input <- readFile "input.txt"
  print (solve input)  
