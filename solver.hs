import System.Environment

isLetter :: Char -> Bool
isLetter ch = ch /= '+' && ch /= '-' && ch /= '*' && ch /= '='

getUniqueLetters :: String -> String
getUniqueLetters str = foldl (\acc x -> if x `notElem` acc then acc ++ [x] else acc) (take 1 str) str

generateNumbers :: Int -> [Int]
generateNumbers n = [10 ^ (n - 1) .. 10 ^ n - 1]

numbersToLists :: [Int] -> [[Int]]
numbersToLists num = map (arrOfDigits) num
  where
    arrOfDigits 0 = []
    arrOfDigits n = arrOfDigits (n `div` 10) ++ [n `mod` 10]

mappingToChars :: [[Int]] -> [[Char]]
mappingToChars lists = map (map (head . show)) lists

-- [11, 12] -> [['1','1'],['1','2']]

distinctNumbers :: [[Char]] -> [[Char]]
distinctNumbers lists = filter (\x -> x == getUniqueLetters x) lists

mergeCharsNumbers :: String -> [[Char]] -> [[(Char, Char)]]
mergeCharsNumbers chars lista = map (zipWith (\a b -> (a, b)) chars) lista

-- "ab" [['1','2'],['1','3']] -> [[('a','1'),('b','2')],[('a','1'),('b','3')]]

generateCombinations :: [String] -> [[(Char, Char)]]
generateCombinations inp = mergeCharsNumbers chars lists
  where
    chars = getUniqueLetters (concat (map (filter isLetter) inp))
    lists = distinctNumbers (mappingToChars (numbersToLists (generateNumbers (length chars))))

replaceOne :: [(Char, Char)] -> [String] -> [String]
replaceOne mapping inp = map (\word -> foldl (\acc a -> map (\z -> if z == fst a then snd a else z) acc) word mapping) inp

replaceLists :: [[(Char, Char)]] -> [String] -> [[String]]
replaceLists mapping str = map (\x -> replaceOne x str) mapping

-- [[('a','1'),('b','2'),('c','3')],[('a','1'),('b','3'),('c','2')]] ["ab","+","c"] -> [["12","+","3"],["13","+","2"]]

calculate :: [String] -> Int
calculate (x : "*" : xs) = read x * calculate xs
calculate (x : "+" : xs) = read x + calculate xs
calculate (x : "-" : xs) = read x - calculate xs
calculate (x : "=" : xs) = read x
calculate _ = 0

rmZeros :: [[String]] -> [[String]]
rmZeros = filter (\a -> foldl (&&) True (map (\x -> take 1 x /= "0") a))

check :: [String] -> [String]
check str = head (filter (\x -> calculate (init x) == read (last x)) (rmZeros (replaceLists (generateCombinations str) str)))

printSolution :: [String] -> IO ()
printSolution strings = putStrLn (foldr (\a b -> a ++ if b == "" then b else " " ++ b) "" strings)

main = do
  (firstArg : _) <- getArgs
  input <- readFile firstArg
  putStrLn input
  printSolution . check . words $ input