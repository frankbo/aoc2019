module DayTwo
  ( dayTwo
  )
where

import           Data.List.Split                ( splitOn )


dayTwo :: IO ()
dayTwo = do
  content <- readFile "./src/input2.txt"
  let c = getFileContent content
  print c

getFileContent :: String -> [Integer]
getFileContent = map read . splitOn ","


calculateNewArray :: [Integer] -> [Integer]
calculateNewArray arr = foldl (\acc val -> acc) [] arr
