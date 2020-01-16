module DayTwo
        ( dayTwo
        , intCodeProgram
        )
where

import           Data.List.Split                ( splitOn )
import qualified Data.Vector                   as V
import           Debug.Trace                    ( trace )

dayTwo :: IO ()
dayTwo = do
        content <- readFile "./src/input2.txt"
        print $ intCodeProgram (getFileContent content)

getFileContent :: String -> [Int]
getFileContent = map read . splitOn ","

intCodeProgram :: [Int] -> [Int]
intCodeProgram input =
        let list         = V.fromList input
            modifiedList = list V.// [(1, 12), (2, 2)]
        in  V.toList (doo modifiedList 0)

doo :: V.Vector Int -> Int -> V.Vector Int
doo list currentPosition =
        let
                opcode = V.take 4 (V.drop currentPosition list)
                zero   = V.unsafeIndex opcode 0
                first  = V.unsafeIndex list (V.unsafeIndex opcode 1)
                second = V.unsafeIndex list (V.unsafeIndex opcode 2)
                idx    = V.unsafeIndex opcode 3
        in
                if currentPosition <= V.length list && zero /= 99
                        then case zero of
                                1 -> doo (add list idx first second)
                                         (currentPosition + 4)
                                2 -> doo
                                        (multiply list idx first second)
                                        (currentPosition + 4)
                                _ -> doo list (currentPosition + 4)
                        else list

-- here is the problem
add :: V.Vector Int -> Int -> Int -> Int -> V.Vector Int
add list index firstNr secondNr = list V.// [(index, firstNr + secondNr)]

multiply :: V.Vector Int -> Int -> Int -> Int -> V.Vector Int
multiply list index firstNr secondNr = list V.// [(index, firstNr * secondNr)]
