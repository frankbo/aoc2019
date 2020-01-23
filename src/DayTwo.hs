module DayTwo
        ( dayTwo
        , intCodeProgram
        )
where

import           Data.List                      ( uncons )
import           Data.List.Split                ( splitOn )
import qualified Data.Vector                   as V
import           Debug.Trace                    ( trace )

dayTwo :: IO ()
dayTwo = do
        content <- readFile "./src/input2.txt"
        print $ case uncons (intCodeProgram (getFileContent content)) of
                Just (x, _) -> Just x
                Nothing     -> Nothing


getFileContent :: String -> [Int]
getFileContent = map read . splitOn ","

intCodeProgram :: [Int] -> [Int]
intCodeProgram input =
        let list = V.fromList input
        in  do
                    noun <- [0 .. 99]
                    verb <- [0 .. 99]
                    True <- return
                            (V.elem 19690720 (findVerbAndNoun verb noun list))
                    return (100 * noun + verb)


findVerbAndNoun :: Int -> Int -> V.Vector Int -> V.Vector Int
findVerbAndNoun verb noun list =
        let modifiedList = list V.// [(1, noun), (2, verb)]
        in  computer modifiedList 0

computer :: V.Vector Int -> Int -> V.Vector Int
computer list currentPosition =
        let
                opcode      = V.take 4 (V.drop currentPosition list)
                instruction = V.unsafeIndex opcode 0
                first       = V.unsafeIndex list (V.unsafeIndex opcode 1)
                second      = V.unsafeIndex list (V.unsafeIndex opcode 2)
                idx         = V.unsafeIndex opcode 3
        in
                if currentPosition <= V.length list && instruction /= 99
                        then case instruction of
                                1 -> computer
                                        (add list idx first second)
                                        (currentPosition + 4)
                                2 -> computer
                                        (multiply list idx first second)
                                        (currentPosition + 4)
                                _ -> computer list (currentPosition + 4)
                        else list

-- here is the problem
add :: V.Vector Int -> Int -> Int -> Int -> V.Vector Int
add list index firstNr secondNr = list V.// [(index, firstNr + secondNr)]

multiply :: V.Vector Int -> Int -> Int -> Int -> V.Vector Int
multiply list index firstNr secondNr = list V.// [(index, firstNr * secondNr)]
