import Data.List (sort)
import System.Environment

data LineStatus = Complete | Incomplete String | Corrupted Char | Error deriving (Show)

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let linesList = lines text
        let corrupted = concat $ map (getCorruptCharacter . getLineStatus) linesList
        print $ sum $ map getPointsA corrupted

        let completions = concat $ map (getCompletion . getLineStatus) linesList
        let scores = sort $ map getScore completions
        print $ getMidOfList scores

getMidOfList :: [a] -> a
getMidOfList xs = (!!) xs (div (length xs - 1) 2)

getPointsA :: Char -> Int
getPointsA ')' = 3
getPointsA ']' = 57
getPointsA '}' = 1197
getPointsA '>' = 25137

getPointsB :: Char -> Int
getPointsB ')' = 1
getPointsB ']' = 2
getPointsB '}' = 3
getPointsB '>' = 4

getScore :: String -> Int
getScore = foldl (\acc c -> acc * 5 + (getPointsB c)) 0

getCorruptCharacter :: LineStatus -> [Char]
getCorruptCharacter (Corrupted c) = [c]
getCorruptCharacter _ = []    

getCompletion :: LineStatus -> [String]
getCompletion (Incomplete s) = [s]
getCompletion _ = []

getLineStatus :: String -> LineStatus
getLineStatus = getLineStat []

getLineStat :: String -> String -> LineStatus
getLineStat [] [] = Complete
getLineStat ys [] = Incomplete (map toMatching ys)
getLineStat [] (x:xs)
    | isOpening x = getLineStat [x] xs
    | otherwise = Error
getLineStat (y:ys) (x:xs)
    | isOpening x = getLineStat (x:y:ys) xs
    | isMatching y x = getLineStat ys xs
    | otherwise = Corrupted x

isOpening :: Char -> Bool
isOpening '[' = True
isOpening '{' = True
isOpening '<' = True
isOpening '(' = True
isOpening _ = False

isMatching :: Char -> Char -> Bool
isMatching c1 c2 = c2 == toMatching c1

toMatching :: Char -> Char
toMatching '[' = ']'
toMatching '<' = '>'
toMatching '{' = '}'
toMatching '(' = ')'