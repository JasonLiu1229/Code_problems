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

checkRange :: (Int, Int) -> Int -> Bool
checkRange (a, b) ingId = a <= ingId && ingId <= b

checkRanges :: [(Int, Int)] -> Int -> Bool
checkRanges [] _ = False
checkRanges (r:rs) ingId = checkRange r ingId || checkRanges rs ingId

convertIds :: [String] -> [Int]
convertIds = map read

countFresh :: [(Int, Int)] -> [Int] -> Int
countFresh ranges ids = length (filter (\i -> checkRanges ranges i) ids)

main :: IO ()
main = do
  ls <- readLines "input.txt"
  let (rangeLines, idLines) = splitLines ls
      ranges = convertRanges rangeLines
      ids    = convertIds idLines
      answer = countFresh ranges ids
  print answer
