import System.Environment
import Data.Text (pack, unpack, splitOn)
import Data.List (sort)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let positions :: [Int] = sort $ map (read . unpack) $ splitOn (pack ",") (pack text)
        let lowest = minimum [calcFuel positions x | x <- [0..(maximum positions)]]
        print $ lowest

        let lowest = minimum [calcFuelComplex positions x | x <- [0..(maximum positions)]]
        print $ lowest

calcFuel :: [Int] -> Int -> Int
calcFuel xs pos = foldl (\acc x -> (+) acc $ abs $ pos - x) 0 xs

calcFuelComplex :: [Int] -> Int -> Int
calcFuelComplex xs pos = foldl (\acc x -> (+) acc $ calcFuelDist $ abs $ pos - x) 0 xs

calcFuelDist :: Int -> Int
calcFuelDist n = (n * (n + 1)) `div` 2

