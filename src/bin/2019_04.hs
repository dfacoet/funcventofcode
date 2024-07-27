import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.List (group)

year :: Int = 2019
day :: Int = 4

main = do
    let filename = printf "input/%d_%02d.txt" year day
    input <- readFile filename
    let parsed_input = parseInput input

    let sol1 = part1 parsed_input
    print sol1
    putStrLn (printf "Part 1: %d" sol1)

    let sol2 = part2 parsed_input
    putStrLn (printf "Part 2: %d" sol2)

parseInput :: String -> (Integer, Integer)
parseInput input = case splitOn "-" input of
    [a, b] -> (read a, read b)
    _ -> error "Invalid input"

hasAdjacentDigits :: Integer -> Bool
hasAdjacentDigits p = any ((>=2) . length) (group (show p))

isIncreasing :: Integer -> Bool
isIncreasing p = all (uncurry (<=)) $ zip (show p) (tail (show p))

isValidPassword1 :: Integer -> Bool
isValidPassword1 p = hasAdjacentDigits p && isIncreasing p

hasTwoAdjacentDigits :: Integer -> Bool
hasTwoAdjacentDigits p = any ((==2) . length) (group (show p))

isValidPassword2 :: Integer -> Bool
isValidPassword2 p = hasTwoAdjacentDigits p && isIncreasing p

part1 :: (Integer, Integer)-> Int
part1 (min, max) = length $ filter isValidPassword1 [min..max]

part2 :: (Integer, Integer)-> Int
part2 (min, max) = length $ filter isValidPassword2 [min..max]

