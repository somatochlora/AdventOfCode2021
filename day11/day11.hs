import Data.List (sort, group)
import Data.Char (digitToInt)
import System.Environment

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let linesList = lines text
        let grid = map (map digitToInt) linesList
        print $ countFlashes grid 100
        print $ (findSync grid 0 0) + 1

countFlashes :: [[Int]] -> Int -> Int
countFlashes grid steps
    | steps <= 0 = 0
    | otherwise = count + countFlashes newGrid (steps - 1)
    where (count, newGrid) = nextTurn grid

findSync :: [[Int]] -> Int -> Int -> Int
findSync grid counter prev
    | count - prev == (height grid) * (width grid) = counter
    | otherwise = findSync newGrid (counter + 1) count
    where (count, newGrid) = nextTurn grid

nextTurn :: [[Int]] -> (Int, [[Int]])
nextTurn grid = (count, twoDimMap (\x -> if x > 9 then 0 else x) newGrid)
    where (count, newGrid) = iterateGrid $ twoDimMap (\x -> (False, x)) $ addOneToAll grid

iterateGrid :: [[(Bool, Int)]] -> (Int, [[Int]])
iterateGrid grid
    | length flashingCoords == 0 = (0, twoDimMap snd grid)
    | otherwise = ((length flashingCoords) + nextNum, nextGrid)
    where   flashingCoords = flashingSquid grid
            flashedCoords = flashedSquid (validCoords grid) flashingCoords            
            newGrid = [[ getNewVal grid flashingCoords flashedCoords (x, y) | x <- [0..width grid - 1]] | y <- [0..height grid - 1]]
            (nextNum, nextGrid) = iterateGrid newGrid

getNewVal :: [[(Bool, Int)]] -> [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> (Bool, Int)
getNewVal grid flashing flashed (x,y)
    | otherwise = (prevB || isFlashing, prevVal + count)
    where   (prevB, prevVal) = grid !! y !! x
            count = length $ filter (==(x, y)) flashed 
            isFlashing = (x, y) `elem` flashing

flashingSquid :: [[(Bool, Int)]] -> [(Int, Int)]
flashingSquid grid = getCoordsCond willFlash grid
  
willFlash :: (Bool, Int) -> Bool
willFlash (already, val)
    | already = False
    | val <= 9 = False
    | otherwise = True

flashedSquid :: ((Int, Int) -> Bool) -> [(Int, Int)] -> [(Int, Int)]
flashedSquid isValid coords = filter isValid $ coords >>= getAdjacentCoords

addOneToAll :: [[Int]] -> [[Int]]
addOneToAll grid = map (map (+1)) grid

getCoordsCond :: (a -> Bool) -> [[a]] -> [(Int, Int)]
getCoordsCond cond grid = [(x, y) | x <- [0..((width grid) - 1)], y <- [0..((height grid) - 1)], cond (grid!!y!!x) ]

getAdjacentCoords :: (Int, Int) -> [(Int, Int)]
getAdjacentCoords (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x, y) /= (x', y')]

validCoords :: [[a]] -> (Int, Int) -> Bool
validCoords grid (x, y)
    | x < 0 || y < 0 || x >= width grid || y >= height grid = False
    | otherwise = True

width :: [[a]] -> Int
width = length . head

height :: [[a]] -> Int
height = length

twoDimMap :: (a -> b) -> [[a]] -> [[b]]
twoDimMap f x = map (map f) x