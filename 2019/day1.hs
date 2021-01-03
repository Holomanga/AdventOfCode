import System.IO ( hClose, openFile, hGetContents, IOMode(ReadMode) )

fuelForModule :: Int -> Int
fuelForModule mass = div mass 3 - 2

recursiveFuelForModule :: Int -> Int
recursiveFuelForModule mass = 
    if fuelForModule mass < 0 then
        0
    else
        fuelForModule mass + recursiveFuelForModule (fuelForModule mass)

part1 :: IO ()
part1 = do
    handle <- openFile "2019/day1.txt" ReadMode
    contents <- hGetContents handle
    print (sum (map (fuelForModule . read) (lines contents) :: [Int]))
    hClose handle

part2 :: IO ()
part2 = do
    handle <- openFile "2019/day1.txt" ReadMode
    contents <- hGetContents handle
    print (sum (map (recursiveFuelForModule . read) (lines contents) :: [Int]))
    hClose handle

main :: IO ()
main = do
    part1
    part2