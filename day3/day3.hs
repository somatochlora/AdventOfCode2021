import System.Environment

main = do
        file <- getArgs 
        textData <- readFile (file!!0)
        let numbers = map binStringToBoolArray $ lines textData
        let starter = replicate (length $ numbers!!0) 0
        let final = foldl addNumbers starter numbers
        let halfLength = (length numbers) `div` 2
        let gamma = map ((<) halfLength) final
        let epsilon = map ((>) halfLength) final
        print $ (boolArrayBinaryToInt gamma) * (boolArrayBinaryToInt epsilon)

        let oxygen = filterNumbers (>=) numbers
        let co2 = filterNumbers (<) numbers
        print $ (boolArrayBinaryToInt oxygen) * (boolArrayBinaryToInt co2)

binStringToBoolArray :: String -> [Bool]
binStringToBoolArray [] = []
binStringToBoolArray (x:xs)
    | x == '1' = True:rest
    | otherwise = False:rest
    where rest = binStringToBoolArray xs

addNumbers :: [Int] -> [Bool] -> [Int]
addNumbers [] [] = []
addNumbers (x:xs) (y:ys)
    | y = (x+1):(addNumbers xs ys)
    | otherwise = (x):(addNumbers xs ys)

boolArrayBinaryToInt :: [Bool] -> Int
boolArrayBinaryToInt = foldl (\prev digit -> 2*prev + (if digit then 1 else 0)) 0

filterNumbers :: (Int -> Int -> Bool) -> [[Bool]] -> [Bool]
filterNumbers _ [x] = x
filterNumbers f xs = (chosen):(filterNumbers f $ map tail remainingOptions)
    where   listLength = length xs
            sumOfHeads = foldl (\acc x -> if head x then acc + 1 else acc) 0 xs
            chosen = if f sumOfHeads (listLength - sumOfHeads) then True else False
            remainingOptions = filter (\x -> head x == chosen) xs

