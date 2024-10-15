import Data.Foldable (minimumBy)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Text.Printf (printf)

year :: Int = 2019

day :: Int = 8

main :: IO ()
main = do
  let filename = printf "input/%d_%02d.txt" year day
  input <- readFile filename
  let parsed_input = parseInput input

  let sol1 = part1 parsed_input
  print sol1
  putStrLn (printf "Part 1: %d" sol1)

  let sol2 = part2 parsed_input
  putStrLn "Part 2: "
  putStrLn sol2

parseInput :: String -> String
parseInput = head . lines

layerHeight :: Int
layerHeight = 6

layerWidth :: Int
layerWidth = 25

part1 :: String -> Int
part1 s = score $ minimumBy nZerosComp $ map countOccurrences $ layers s
  where
    nZerosComp = comparing (Map.findWithDefault 0 '0')
    score counts = Map.findWithDefault 0 '1' counts * Map.findWithDefault 0 '2' counts

part2 :: String -> String
part2 s = unlines $ reshape layerWidth $ map getFirstPixel $ transpose $ layers s
  where
    getFirstPixel sec = case head $ filter (/= '2') sec of
      '0' -> ' '
      '1' -> '█'

reshape :: Int -> [a] -> [[a]]
reshape _ [] = []
reshape n xs
  | n > length xs = error "length of list not divisible by n"
  | otherwise = take n xs : reshape n (drop n xs)

layers :: [a] -> [[a]]
layers = reshape (layerHeight * layerWidth)

countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)
