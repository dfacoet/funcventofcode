import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.List (partition)

year :: Int = 2019
day :: Int = 3

main = do
    let filename = printf "input/%d_%02d.txt" year day
    input <- readFile filename
    let parsed_input = parseInput input

    let first_wire = expandWire (head parsed_input)

    print first_wire



    -- let sol1 = part1 parsed_input
    -- putStrLn (printf "Part 1: %d" sol1)

    -- let sol2 = part2 parsed_input
    -- putStrLn (printf "Part 2: %d" sol2)

parseInput :: String -> [[String]]
parseInput input = map (splitOn ",") (lines input)

wireStep :: String -> (Int, Int) -> (Int, Int)
wireStep step (x, y) = case head step of
    'U' -> (x, y + read (tail step))
    'D' -> (x, y - read (tail step))
    'L' -> (x - read (tail step), y)
    'R' -> (x + read (tail step), y)

expandWire :: [String] -> [(Int, Int)]
-- returns a list of all endpoints of the wire
expandWire = foldl (\acc step -> acc ++ [wireStep step (last acc)]) [(0, 0)]

intersectSegments :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersectSegments (x1, y1) (x2, y2) (x3, y3) (x4, y4) = 
    let (x1', x2') = (min x1 x2, max x1 x2)
        (y1', y2') = (min y1 y2, max y1 y2)
        (x3', x4') = (min x3 x4, max x3 x4)
        (y3', y4') = (min y3 y4, max y3 y4)
    in if x1' <= x3' && x3' <= x2' && y3' <= y1' && y1' <= y4'
        then Just (x3', y1')
        else Nothing

manhattanNorm :: (Int, Int) -> Int
manhattanNorm (x, y) = abs x + abs y

part1 :: [[String]] -> Int
--- expand both wires
--- iterate over all segments (pair of consecutive endpoints) of both wires
--- find intersections
--- apply Manhattan norm
--- return min