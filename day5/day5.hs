import System.Environment
import Data.Text (pack, unpack, splitOn)
import Data.List (group, sort)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let lineNums = filter isOrthogonal $ map parseLine $ lines text
        let occupied = concat $ map getCoords lineNums
        let badSpaces = filter ((>1) . length) $ group $ sort occupied
        print $ length badSpaces

        let lineNums = map parseLine $ lines text
        let occupied = concat $ map getCoords lineNums
        let badSpaces = filter ((>1) . length) $ group $ sort occupied
        print $ length badSpaces


isOrthogonal :: ((Int, Int), (Int, Int)) -> Bool
isOrthogonal ((x1, y1),(x2, y2))
    | (x1 - x2 /= 0) && (y1 - y2 /= 0) = False
    | otherwise = True

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s = (parseCoords $ segments!!0, parseCoords $ segments!!2)
    where segments = words s

parseCoords :: String -> (Int, Int)
parseCoords s = (read $ segments!!0, read $ segments!!1)
    where segments = map unpack $ splitOn (pack ",") $ pack s

lineDirection :: ((Int, Int), (Int, Int)) -> (Int, Int)
lineDirection ((x1, y1),(x2, y2)) = (calcDirection x1 x2, calcDirection y1 y2)

getCoords :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getCoords (x, y) | x == y = [x]
getCoords line@((x1, y1),(x2, y2)) = (x1, y1):(getCoords ((x1 + xAdd, y1 + yAdd), (x2, y2)))
    where   xAdd = fst $ lineDirection line
            yAdd = snd $ lineDirection line

calcDirection :: Int -> Int -> Int
calcDirection x y
    | x > y = -1
    | x < y = 1
    | otherwise = 0