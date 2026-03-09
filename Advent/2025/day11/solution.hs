-- ------------------------------------------------------------
-- Graph type
--
-- A graph is represented as a list of pairs:
--   (nodeName, [list of outgoing neighbours])
--
-- Example:
--   [("you", ["a", "b"]), ("a", ["out"]), ("b", ["out"])]
-- ------------------------------------------------------------
type Graph = [(String, [String])]


-- ------------------------------------------------------------
-- readLines :: FilePath -> IO [String]
--
-- Reads a file and returns its contents as a list of lines.
--
-- readFile reads the whole file as one String.
-- lines splits that String at newline characters.
-- <$> applies lines inside the IO result of readFile.
-- ------------------------------------------------------------
readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path


-- ------------------------------------------------------------
-- parseLine :: String -> (String, [String])
--
-- Converts one line of the input into one graph entry.
--
-- Expected format of a line:
--   "you: a b c"
--
-- Steps:
-- 1. words line splits the line into pieces:
--      ["you:", "a", "b", "c"]
-- 2. head ws gets the first word: "you:"
-- 3. init removes the last character ':' -> "you"
-- 4. tail ws gets the rest of the words:
--      ["a", "b", "c"]
--
-- Result:
--   ("you", ["a", "b", "c"])
-- ------------------------------------------------------------
parseLine :: String -> (String, [String])
parseLine line =
  let ws      = words line           -- split string into words separated by spaces
      name    = init (head ws)       -- remove the trailing ':'
      outputs = tail ws              -- all remaining words are outgoing neighbours
  in (name, outputs)


-- ------------------------------------------------------------
-- neighbours :: Graph -> String -> [String]
--
-- Returns the outgoing neighbours of a given node.
--
-- lookup node graph searches for the node in the graph.
--
-- Cases:
-- - Nothing   -> the node is not in the graph, so return []
-- - Just outs -> return its list of neighbours
--
-- Example:
--   neighbours [("you", ["a","b"])] "you" = ["a","b"]
--   neighbours [("you", ["a","b"])] "x"   = []
-- ------------------------------------------------------------
neighbours :: Graph -> String -> [String]
neighbours graph node =
  case lookup node graph of
    Nothing   -> []
    Just outs -> outs


-- ------------------------------------------------------------
-- countPaths :: Graph -> String -> String -> Int
--
-- Counts the number of paths from the current node to the target node.
--
-- Parameters:
-- - graph   : the full graph
-- - current : the node where we are now
-- - target  : the destination node
--
-- Logic:
-- 1. If current == target, then we found one complete path,
--    so return 1.
-- 2. Otherwise:
--    - get all neighbours of current
--    - recursively count paths from each neighbour to target
--    - sum all those counts
--
-- Example:
--   Graph:
--     you -> a, b
--     a   -> out
--     b   -> out
--
--   countPaths graph "you" "out"
--   = countPaths graph "a" "out" + countPaths graph "b" "out"
--   = 1 + 1
--   = 2
-- ------------------------------------------------------------
countPaths :: Graph -> String -> String -> Int
countPaths graph current target
  | current == target = 1
  | otherwise         = sum (map (\n -> countPaths graph n target) ns) -- given n, count the paths known graph and target
  where
    ns = neighbours graph current


-- ------------------------------------------------------------
-- main :: IO ()
--
-- Program entry point.
--
-- Steps:
-- 1. Read all lines from input.txt
-- 2. Convert each line into one graph entry using parseLine
-- 3. Count the number of paths from "you" to "out"
-- 4. Print the result
-- ------------------------------------------------------------
main = do
  ls <- readLines "input.txt"
  let graph  = map parseLine ls
      result = countPaths graph "you" "out"
  putStrLn ("Total paths: " ++ show result)
