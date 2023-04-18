import qualified Prelude
import Prelude hiding (words)
import Data.List hiding (words)
import Data.Text hiding (map, head, tail, transpose, filter, length, concat)
import System.Environment

main = do
        file <- getArgs
        textData <- readFile (file!!0)
        let numbers :: [Int] = map (read . unpack) $ splitOn (pack ",") $ head $ splitOn (pack "\n\n") (pack textData)
        let boards = map parseBoard $ tail $ splitOn (pack "\n\n") (pack textData)
        let winner = playGame numbers boards
        print $ calcScore winner
        let loser = playLosingGame numbers boards
        print $ calcScore loser

calcScore :: ([[(Bool, Int)]], Int) -> Int
calcScore (board, n) = sumOfUnmarked * n
    where sumOfUnmarked = sum $ map (\(a,b) -> if a then 0 else b) $ concat board

playGame :: [Int] -> [[[(Bool, Int)]]] -> ([[(Bool, Int)]], Int)
playGame (x:xs) bs = case winner of
    Nothing -> playGame xs $ newBoards
    Just board -> (board, x)
    where   newBoards = playRound x bs
            winner = getWinner newBoards

playLosingGame :: [Int] -> [[[(Bool, Int)]]] -> ([[(Bool, Int)]], Int)
playLosingGame (x:xs) bs
    | length notWonYet == 0 = (head newBoards, x)
    | otherwise = playLosingGame xs notWonYet
    where   newBoards = playRound x bs
            notWonYet = filter (not . checkBoard) newBoards

playRound :: Int -> [[[(Bool, Int)]]] -> [[[(Bool, Int)]]]
playRound n = map (markSquare n)

getWinner :: [[[(Bool, Int)]]] -> Maybe [[(Bool, Int)]]
getWinner bs
    | length winner == 0 = Nothing
    | otherwise = Just (head winner)
    where winner = filter checkBoard bs

parseBoard :: Text -> [[(Bool, Int)]]
parseBoard s = twoDimMap ((tupleFromEles False) . read . unpack) eles
    where eles = map (words) $ splitOn (pack "\n") s
    
tupleFromEles :: a -> b -> (a, b)
tupleFromEles a b = (a, b)

twoDimMap :: (a -> b) -> [[a]] -> [[b]]
twoDimMap f x = map (map f) x

checkBoard :: [[(Bool, Int)]] -> Bool
checkBoard xs = or $ 
    (map and boolsOnly) --rows
    ++ (map and $ transpose boolsOnly) --cols
    where boolsOnly = map (map fst) xs

markSquare :: Int -> [[(Bool, Int)]] -> [[(Bool, Int)]]
markSquare n = twoDimMap (setNewVal n)

setNewVal :: Int -> (Bool, Int) -> (Bool, Int)
setNewVal val (marked, num)
    | val == num = (True, num)
    | otherwise = (marked, num)
