module Lib
  ( dayOne
  ) where

dayOne :: IO ()
dayOne = do
  words <- getWords "./src/input.txt"
  print $ fuelForModules (map read words)

getWords :: FilePath -> IO [String]
getWords path = do
  contents <- readFile path
  return (lines contents)

fuelForModules :: [Integer] -> Integer
fuelForModules =
  foldl (\acc current -> acc + fuelForFuel (calculateFuel current)) 0

calculateFuel :: Integer -> Integer
calculateFuel fuel = (fuel `div` 3) - 2

fuelForFuel :: Integer -> Integer
fuelForFuel fuel
  | calculateFuel fuel <= 0 = fuel
  | otherwise = fuel + fuelForFuel (calculateFuel fuel)
