import System.Environment
import Data.Char (digitToInt)
import Data.List (group, sort)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let grid = parseGrid text
        let coords = allCoords grid
        let lowPoints = filter (flip isLowPoint $ grid) coords
        print $ sum $ map (flip getRiskScore $ grid) lowPoints

        -- just did this to display it clearly but ended up being useful for solving
        let displayGridSetup = twoDimZip (lines text) (allCoordsTwoD grid)
        let displayGrid = twoDimMap ((flip replaceChar) lowPoints) displayGridSetup
        writeFile "display.txt" (unlines displayGrid)        

        let basins = map ((flip expandArea) displayGrid) $ map return lowPoints
        print $ multiplyThreeLargest $ map length basins

multiplyThreeLargest :: [Int] -> Int
multiplyThreeLargest xs = f $ reverse $ sort xs
    where f (x:y:z:xs) = x * y * z

replaceChar :: (Char, (Int, Int)) -> [(Int, Int)] -> Char
replaceChar ('9', _) _ = '#'
replaceChar (c, coord) lowPoints
    | coord `elem` lowPoints = 'o'
    | otherwise = ' '

twoDimZip :: [[a]] -> [[b]] -> [[(a,b)]]
twoDimZip = zipWith zip

allCoords :: [[a]] -> [(Int, Int)]
allCoords grid = [(x, y) | x <- [0.. (subtract 1) $ width grid], y <- [0.. (subtract 1) $ height grid]]

allCoordsTwoD :: [[a]] -> [[(Int, Int)]]
allCoordsTwoD grid = [[(x, y) | x <- [0.. (subtract 1) $ width grid]] | y <- [0.. (subtract 1) $ height grid]]

parseGrid :: String -> [[Int]]
parseGrid s = twoDimMap digitToInt $ lines s

twoDimMap :: (a -> b) -> [[a]] -> [[b]]
twoDimMap f x = map (map f) x

getRiskScore :: (Int, Int) -> [[Int]] -> Int
getRiskScore coords grid = 1 + getVal coords grid

isLowPoint :: (Int, Int) -> [[Int]] -> Bool
isLowPoint (x, y) grid = isLowerThan (x+1, y) && isLowerThan (x-1, y) && isLowerThan (x, y+1) && isLowerThan (x, y-1)
    where isLowerThan = isLower (x, y) grid

isLower :: (Int, Int) -> [[Int]] -> (Int, Int)  -> Bool
isLower (x1, y1) grid (x2, y2)
    | (x2 < 0) || (x2 >= width grid) || (y2 < 0) || (y2 >= height grid) = True
    | otherwise = (getVal (x1, y1) grid) < (getVal (x2, y2) grid)

width :: [[a]] -> Int
width = length . head

height :: [[a]] -> Int
height = length

getVal :: (Int, Int) -> [[a]] -> a
getVal (x, y) grid = grid!!y!!x

expandArea :: [(Int, Int)] -> [[Char]] -> [(Int, Int)]
expandArea current grid
    | (length new) == 0 = current
    | otherwise = expandArea (current ++ new) grid
    where   adjacent = adjacentCoordsArea current grid
            new = filter (isEmpty grid) adjacent

isEmpty :: [[Char]] -> (Int, Int) -> Bool
isEmpty grid coord = (getVal coord grid) == ' '

adjacentCoordsArea :: [(Int, Int)] -> [[a]] -> [(Int, Int)]
adjacentCoordsArea cur grid = filter ((flip notElem) cur) $ filter (withinGrid grid) $ getUnique $ (concat (map adjacentCoords cur))

getUnique :: (Eq a, Ord a) => [a] -> [a]
getUnique xs = map head $ group $ sort xs

adjacentCoords :: (Int, Int) -> [(Int, Int)]
adjacentCoords (x, y) = (x+1,y):(x-1,y):(x,y+1):(x,y-1):[]

withinGrid :: [[a]] -> (Int, Int) -> Bool
withinGrid grid (x,y) = (x >= 0) && (x < width grid) && (y >= 0) && (y < height grid)