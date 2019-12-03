module DayOne
  ( dayOne
  ) where

dayOne :: IO ()
dayOne = do
  words <- getWords "./src/input.txt"
  print $ fuelForModules (map read words)

getWords :: FilePath -> IO [String]
getWords = fmap lines . readFile

fuelForModules :: [Integer] -> Integer
fuelForModules =
  foldl (\acc current -> acc + fuelForFuel (calculateFuel current)) 0

calculateFuel :: Integer -> Integer
calculateFuel fuel = (fuel `div` 3) - 2

fuelForFuel :: Integer -> Integer
fuelForFuel fuel
  | calcFuel <= 0 = fuel
  | otherwise = fuel + fuelForFuel calcFuel
  where
    calcFuel = calculateFuel fuel
