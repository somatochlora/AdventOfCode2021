import System.Environment
import Data.Text (pack, unpack, splitOn)
import qualified Data.Map as Map

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let starters = map (read. unpack) $ splitOn (pack ",") (pack text)
        let startMap = foldl addVal (Map.fromList $ zip [0..8] (replicate 9 0)) starters
        let finalMap = chain doStep 80 startMap
        print $ sum $ Map.elems finalMap

        let finalMap = chain doStep 256 startMap
        print $ sum $ Map.elems finalMap

doStep :: Map.Map Int Int -> Map.Map Int Int
doStep m = Map.fromList $ zip [0..8]
    [getVal 1 m,
    getVal 2 m,
    getVal 3 m, 
    getVal 4 m,
    getVal 5 m, 
    getVal 6 m,
    (getVal 7 m) + (getVal 0 m),
    getVal 8 m,
    getVal 0 m]

getVal :: Int -> Map.Map Int Int -> Int
getVal x m = case Map.lookup x m of
    Nothing -> 0
    Just n -> n

addVal :: Map.Map Int Int -> Int -> Map.Map Int Int
addVal fishMap x = case cur of
    Nothing -> Map.insert x 1 fishMap
    Just n -> Map.insert x (n+1) fishMap
    where cur = Map.lookup x fishMap 

chain :: (a -> a) -> Int -> (a -> a)
chain f 0 = (\x -> x)
chain f n = f . chain f (n-1)



        