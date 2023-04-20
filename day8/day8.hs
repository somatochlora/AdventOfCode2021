import System.Environment
import Data.Text (pack, unpack, splitOn)
import Data.List (sort, group)
import Data.Maybe (fromMaybe)

-- the example was formatted differently than the input... so I just modified the input to match the example

main = do
        file <- getArgs
        text <- readFile (file!!0)
        let rows = lines text
        let displays = [(init $ words $ rows!!x, words $ rows!!(x + 1)) | x <- [0..((subtract 1) $ length rows)], even x]
        let outputAll = concat $ map snd displays
        print $ length $ filter (flip elem [2, 4, 3, 7] . length) outputAll

        let displayOutputs = map decodeDisplay displays
        print $ sum displayOutputs

decodeDisplay :: ([String], [String]) -> Int
decodeDisplay (input, output) = digitListToInt digits
        where   key = parseLetters input
                digits = map (fromMaybe (-999999)) $ map ((flip lookup) key) $ map sort output
                
digitListToInt :: [Int] -> Int
digitListToInt xs = foldl (\acc x -> acc * 10 + x) 0 xs

parseLetters :: [String] -> [(String, Int)]
parseLetters xsUnsorted = zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..9]
        where   xs = map sort xsUnsorted
                one = fromMaybe "one" $ headIfOnly $ filterByLength 2 xs
                four = fromMaybe "four" $ headIfOnly $ filterByLength 4 xs
                seven = fromMaybe "seven" $ headIfOnly $ filterByLength 3 xs
                eight = fromMaybe "eight" $ headIfOnly $ filterByLength 7 xs
                bottomLeft = fromMaybe 'z' $ getUniqueEle $ concat $ filterByLengths [4,5] xs
                two = fromMaybe "two" $ headIfOnly $ filterElem bottomLeft $ filterByLength 5 xs
                three = fromMaybe "three" $ headIfOnly $ filterElem (head one) $ filterElem (last one) $ filterByLength 5 xs
                five = fromMaybe "five" $ headIfOnly $ filter (/= two) $ filter (/= three) $ filterByLength 5 xs
                nine = fromMaybe "nine" $ headIfOnly $ filterNotElem bottomLeft $ filterByLength 6 xs
                topRight = fromMaybe 'z' $ getUniqueEle $ five ++ nine
                six = fromMaybe "six" $ headIfOnly $ filterNotElem topRight $ filterByLength 6 xs
                zero = fromMaybe "zero" $ headIfOnly $ filter (/= nine) $ filter (/= six) $ filterByLength 6 xs
                

getUniqueEle :: (Ord a) => [a] -> Maybe a
getUniqueEle xs 
        | length uniques == 1 = Just $ head $ head $ uniques
        | otherwise = Nothing
        where uniques = filterByLength 1 $ group $ sort xs

lengthIs :: Int -> [a] -> Bool
lengthIs n x = length x == n 

headIfOnly :: [a] -> Maybe a
headIfOnly [x] = Just x
headIfOnly _ = Nothing

filterElem :: (Eq a) => a -> [[a]] -> [[a]]
filterElem ele xs = filter (elem ele) xs

filterNotElem :: (Eq a) => a -> [[a]] -> [[a]]
filterNotElem ele xs = filter (not . elem ele) xs

filterByLength :: Int -> [[a]] -> [[a]]
filterByLength n xs = filter (lengthIs n) xs

filterByLengths :: [Int] -> [[a]] -> [[a]]
filterByLengths [] _ = []
filterByLengths (n:ns) xs = (filterByLength n xs) ++ (filterByLengths ns xs) 
