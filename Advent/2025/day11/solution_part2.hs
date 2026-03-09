import Data.Map (Map)
import qualified Data.Map as Map

-- ------------------------------------------------------------
-- Graph type
--
-- A graph is represented as a list of pairs:
--   (nodeName, [list of outgoing neighbours])
--
-- Example:
--   [("svr", ["a", "b"]), ("a", ["out"]), ("b", ["out"])]
-- ------------------------------------------------------------
type Graph = [(String, [String])]


-- ------------------------------------------------------------
-- Cache type
--
-- Maps (nodeName, seenDac, seenFft) -> count of valid paths.
--
-- The key has three parts because the same node can be reached
-- with different histories (have we seen dac? have we seen fft?).
-- Each combination gives a different path count so needs its own entry.
-- ------------------------------------------------------------
type Cache = Map (String, Bool, Bool) Int


-- ------------------------------------------------------------
-- readLines :: FilePath -> IO [String]
--
-- Reads a file and returns its contents as a list of lines.
-- readFile reads the whole file as one String.
-- lines splits that String at newline characters.
-- <$> applies lines inside the IO result of readFile.
-- ------------------------------------------------------------
readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path


-- ------------------------------------------------------------
-- parseLine :: String -> (String, [String])
--
-- Converts one line like "svr: a b c"
-- into a graph entry ("svr", ["a","b","c"]).
--
-- Steps:
--   words "svr: a b c" = ["svr:", "a", "b", "c"]
--   init (head ws)     = "svr"   (drops the ':')
--   tail ws            = ["a", "b", "c"]
-- ------------------------------------------------------------
parseLine :: String -> (String, [String])
parseLine line =
  let ws      = words line
      name    = init (head ws)   -- drop the trailing ':'
      outputs = tail ws          -- all remaining words are neighbours
  in (name, outputs)


-- ------------------------------------------------------------
-- neighbours :: Graph -> String -> [String]
--
-- Returns the outgoing neighbours of a node.
-- Returns [] if the node is not in the graph (e.g. "out").
-- ------------------------------------------------------------
neighbours :: Graph -> String -> [String]
neighbours graph node =
  case lookup node graph of
    Nothing   -> []
    Just outs -> outs


-- ------------------------------------------------------------
-- countValidPaths :: Graph -> Cache -> String -> Bool -> Bool -> (Int, Cache)
--
-- Counts paths from current to "out" that visit both "dac" and "fft".
-- Returns (count, updatedCache) so the cache grows as we explore.
--
-- Parameters:
--   graph    : the full graph
--   cache    : previously computed results
--   current  : node we are at right now
--   seenDac  : have we passed through "dac" on this path so far?
--   seenFft  : have we passed through "fft" on this path so far?
--
-- Logic:
-- 1. Reached "out": return 1 if both flags are True, else 0.
-- 2. Cache hit: if (current, seenDac, seenFft) is already stored,
--    return that result immediately without further exploration.
-- 3. Cache miss: explore all neighbours threading the cache through
--    foldl, then store the result before returning.
-- ------------------------------------------------------------
countValidPaths :: Graph -> Cache -> String -> Bool -> Bool -> (Int, Cache)
countValidPaths graph cache current seenDac seenFft
  | current == "out" = (if seenDac && seenFft then 1 else 0, cache)
  | otherwise =
      case Map.lookup key cache of
        Just n  -> (n, cache)    -- cache hit: return stored result immediately
        Nothing ->               -- cache miss: must compute it
          let
              seenDac' = seenDac || current == "dac"
              seenFft' = seenFft || current == "fft"

              ns = neighbours graph current

              -- foldl processes each neighbour one by one
              -- carrying (runningTotal, cache) forward at each step
              -- so each neighbour benefits from cache entries added by previous neighbours
              --
              -- \(acc, c) n ->
              --   acc = running total so far
              --   c   = cache so far
              --   n   = current neighbour being processed
              (total, cache') = foldl (\(acc, c) n ->
                                    let (v, c') = countValidPaths graph c n seenDac' seenFft'
                                    --   ^result  ^updated cache from going deeper
                                    in  (acc + v, c'))  -- add result, pass cache forward
                                  (0, cache)            -- start: total=0, current cache
                                  ns                    -- fold over all neighbours

          in (total, Map.insert key total cache')
          --          store this node's result so future visits can skip recomputing it
  where
    key = (current, seenDac, seenFft)


-- ------------------------------------------------------------
-- Steps:
-- 1. Read all lines from input.txt
-- 2. Parse into a graph
-- 3. Count valid paths starting with empty cache and both flags False
-- 4. Print the result
-- ------------------------------------------------------------
main = do
  ls <- readLines "input.txt"
  let graph       = map parseLine ls
      (result, _) = countValidPaths graph Map.empty "svr" False False
      --                                  ^^^^^^^^^
      --                                  start with empty cache, nothing computed yet
  putStrLn ("Paths visiting dac and fft: " ++ show result)
