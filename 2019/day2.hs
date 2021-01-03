import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

import qualified Data.Text
split :: String -> String -> [String]
split s1 s2 = map Data.Text.unpack ( Data.Text.splitOn (Data.Text.pack s1) (Data.Text.pack s2))

setValueAtIndex :: [Int] -> Int -> Int -> [Int]
setValueAtIndex array index value = take index array ++ [value] ++ drop (index + 1) array

stepIntcode :: [Int] -> Int -> [Int]
stepIntcode computer index
    | computer!!index == 1 =
        stepIntcode (setValueAtIndex computer (computer!!(index+3)) (computer!!(computer!!(index+2)) + computer!!(computer!!(index+1)))) (index+4)
    | computer!!index == 2 = 
        stepIntcode (setValueAtIndex computer (computer!!(index+3)) (computer!!(computer!!(index+2)) * computer!!(computer!!(index+1)))) (index+4)
    | computer!!index == 99 = computer
    | otherwise = error "Invalid operation."

part1 :: IO ()
part1 = do
    handle <- openFile "2019/day2.txt" ReadMode
    contents <- hGetContents handle
    let computer = map read (split "," contents) :: [Int]
    let newComputer = setValueAtIndex (setValueAtIndex computer 1 12) 2 2
    print (stepIntcode newComputer 0)

main :: IO ()
main = do
    part1