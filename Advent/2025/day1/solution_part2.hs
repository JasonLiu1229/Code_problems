-- Read file from filepath and return lines 
readLines :: FilePath -> IO [String] -- Takes in a filepath and returns lines in the form list IO 
readLines path = lines <$> readFile path

-- Parse direction of the string in the format 'R12'
getDirection :: String -> Char -- Takes input string of format 'R12' and return 'R'
getDirection x = head x

-- Parse amount of the string in the format 'R12'
getAmount :: String -> Int -- Takes input string of format 'R12' and return 12 
getAmount x = read (tail x)

-- Applies the rotation on the current dial 
applyRotation :: Int -> Char -> Int -> Int -- Takes in the current dial position, the direction and the amount it needs to twist and outputs the new dial position
applyRotation dial direction amount
  | direction == 'R' = (dial + amount) `mod` 100
  | otherwise        = (dial - amount) `mod` 100

-- Counts how far we are from zero, if we have hit zero and if so count that  
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

-- Simple solve function where we combine everything
solve :: [String] -> Int -> Int -> Int -- Takes the rotations we will apply, the current dial position, and the count that we are currently add. It returns the count that we finish at.
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
