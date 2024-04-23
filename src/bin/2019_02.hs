import Text.Printf (printf)
import Data.List.Split (splitOn)

year :: Int = 2019
day :: Int = 2

main = do
    let filename = printf "input/%d_%02d.txt" year day
    input <- readFile filename
    let parsed_input = parseInput input

    putStrLn (unwords (map show parsed_input))

    let sol1 = part1 parsed_input
    putStrLn (printf "Part 1: %d" sol1)

    -- let sol2 = part2 parsed_input
    -- putStrLn (printf "Part 2: %d" sol2)

parseInput :: String -> [Int]
parseInput input = map read (splitOn "," input)

part1 :: [Int] -> Int
part1 program = head $ runProgram program

-- 116 is too low

runProgram :: [Int] -> [Int]
runProgram program = go 0 program
    where
        go i program
            | i >= length program = program
            | otherwise = go i' program'
            where
                (i', program') = step i program

step :: Int -> [Int] -> (Int, [Int])
step i program = case program !! i of
    1 -> (i + 4, replaceElement (program !! (i + 3)) (program !! (i + 1) + program !! (i + 2) ) program)
    2 -> (i + 4, replaceElement (program !! (i + 3)) (program !! (i + 1) * program !! (i + 2) ) program)
    99 -> (length program, program)
    _ -> error "Invalid opcode"

replaceElement :: Int -> Int -> [Int] -> [Int]
replaceElement i x xs = take i xs ++ [x] ++ drop (i + 1) xs
-- part2 :: [Int] -> Int
-- part2 program = 0
