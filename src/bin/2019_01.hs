import Text.Printf (printf)

year :: Int = 2019
day :: Int = 1

main = do
    let filename = printf "input/%d_%02d.txt" year day
    input <- readFile filename
    let parsed_input = parseInput input

    let sol1 = part1 parsed_input
    putStrLn (printf "Part 1: %d" sol1)

    let sol2 = part2 parsed_input
    putStrLn (printf "Part 2: %d" sol2)

parseInput :: String -> [Int]
parseInput input = map read (lines input)

part1 :: [Int] -> Int
part1 masses = sum ( map fuel masses)

part2 :: [Int] -> Int
part2 masses = sum ( map totalFuel masses)

fuel :: Int -> Int
fuel mass = div mass 3 - 2

totalFuel :: Int -> Int
totalFuel mass = sum (takeWhile (>0) (tail (iterate fuel mass)))