main = do  
        contents <- readFile "data.txt"
        let instructions = map parseInstruction $ lines contents
        let finalCoord = foldl addInstruction (0, 0) instructions
        print $ calcOutput finalCoord
        let finalCoordAim = foldl addInstructionAim (0, 0, 0) instructions
        print $ calcOutputAim finalCoordAim

parseInstruction :: String -> (String, Int)
parseInstruction s =
    let bits = words s
        ins = bits!!0
        num = bits!!1
    in (ins, read num)

addInstruction :: (Int, Int) -> (String, Int) -> (Int, Int)
addInstruction (x, y) (ins, n) 
    | ins == "forward" = (x + n, y)
    | ins == "down" = (x, y + n)
    | ins == "up" = (x, y - n)

addInstructionAim :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
addInstructionAim (x, y, aim) (ins, n) 
    | ins == "forward" = (x + n, y + (aim * n), aim)
    | ins == "down" = (x, y, aim + n)
    | ins == "up" = (x, y, aim - n)

calcOutput :: (Int, Int) -> Int
calcOutput (x, y) = x * y

calcOutputAim :: (Int, Int, Int) -> Int
calcOutputAim (x, y, _) = x * y