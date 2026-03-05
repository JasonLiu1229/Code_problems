-- Read file and convert it to lines 
readLines :: FilePath -> IO [String] -- Takes in a file path and converts it to IO list of strings
readLines path = lines <$> readFile path

-- Parse the string to get the direction
getDirection :: String -> Char -- Takes input of the format 'R12' and outputs first char 'R'
getDirection x = head x -- the string is in the format 'R12' so we take the first char 

-- Parse the string to get the amount to twist the dial, we also convert it to int
getAmount :: String -> Int -- Takes input of the format 'R12' and outputs 12 
getAmount x = read (tail x)

-- Applying rotation based on the direction given
applyRotation :: Int -> Char -> Int -> Int -- Takes in the current dial position, the direction and the amount to twist it 
applyRotation dial direction amount
  | direction == 'R' = mod (dial + amount) 100
  | otherwise = mod (dial - amount) 100

-- Check if dial is at 0, if so we just return an int 1 so we can add this to the count
checkDial :: Int -> Int -- Takes in the current dial to just check if it is 0
checkDial dial
  | dial == 0 = 1
  | otherwise = 0

-- Main solve function that combines everything
solve :: [String] -> Int -> Int -> Int -- Takes in the list of rotations the current dial and the aount it had hit 0
solve rotations dial count 
  | rotations == [] = count
  | otherwise = solve ( tail rotations ) 
                      ( applyRotation dial (getDirection (head rotations) ) ( getAmount ( head rotations ) ) ) 
                      ( count + (checkDial (  applyRotation dial ( getDirection ( head rotations ) ) ( getAmount ( head rotations ) ) ) ) )

main = do
  rotations <- readLines "day1/input_hs.txt"
  print (solve rotations 50 0)
