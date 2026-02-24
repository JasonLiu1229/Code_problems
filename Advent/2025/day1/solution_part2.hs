readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

getDirection :: String -> Char
getDirection x = head x

getAmount :: String -> Int
getAmount x = read (tail x)

applyRotation :: Int -> Char -> Int -> Int
applyRotation dial direction amount
  | direction == 'R' = (dial + amount) `mod` 100
  | otherwise        = (dial - amount) `mod` 100

countZeros :: Int -> Char -> Int -> Int
countZeros dial direction amount
  | amount <= 0 = 0 -- avoid rotations on zero or negative number, so we do not do pointless clicks
  | otherwise =
      let firstHit = -- how many clicks before we hit our first 0
            if direction == 'R'
              then let t = (100 - dial) `mod` 100
                in if t == 0 then 100 else t
            else let t = dial `mod` 100
              in if t == 0 then 100 else t
      in if firstHit > amount -- if the amount > firsthit, then this means we did hit at least on click, if first hit is greather, this means we need more steps before we ever reach 0
           then 0
           else 1 + (amount - firstHit) `div` 100

solve :: [String] -> Int -> Int -> Int
solve rotations dial count
  | null rotations = count
  | otherwise =
      let r     = head rotations
          dir   = getDirection r
          amt   = getAmount r
          hits  = countZeros dial dir amt
          dial' = applyRotation dial dir amt
      in solve (tail rotations) dial' (count + hits)

main :: IO ()
main = do
  rotations <- readLines "day1/input_hs.txt"
  print (solve rotations 50 0)
