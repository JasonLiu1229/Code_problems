readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path

getDirection :: String -> Char
getDirection x = head x

getAmount :: String -> Int 
getAmount x = read (tail x)

applyRotation :: Int -> Char -> Int -> Int
applyRotation dial direction amount
  | direction == 'R' = mod (dial + amount) 100
  | otherwise = mod (dial - amount) 100

checkDial :: Int -> Int
checkDial dial
  | dial == 0 = 1
  | otherwise = 0

solve :: [String] -> Int -> Int -> Int
solve rotations dial count 
  | rotations == [] = count
  | otherwise = solve ( tail rotations ) 
                      ( applyRotation dial (getDirection (head rotations) ) ( getAmount ( head rotations ) ) ) 
                      ( count + (checkDial (  applyRotation dial ( getDirection ( head rotations ) ) ( getAmount ( head rotations ) ) ) ) )

main = do
  rotations <- readLines "day1/input_hs.txt"
  print (solve rotations 50 0)
