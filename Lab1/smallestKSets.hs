-- Lab 1

-- Function to retrieve all subsets. 
allContiguousSublists :: [Int] -> [([Int], (Int, Int))]
allContiguousSublists xs =
  [ (take (j - i + 1) (drop i xs), (i+1, j+1))
  | i <- [0 .. length xs - 1]
  , j <- [i .. length xs - 1]
  ]


-- Function to calculate the subset sums
calcSubsetSum :: [([Int], (Int, Int))] -> [(Int, [Int], Int, Int)]
calcSubsetSum subsets = [(sum subset, subset, i, j) | (subset, (i, j)) <- subsets]

-- Function to sort subsets in regards to their sum
sortSubsets :: [(Int, [Int], Int, Int)] -> [(Int, [Int], Int, Int)]
sortSubsets [] = []
sortSubsets ((sum, sub, i, j):xs) =
  let smaller = sortSubsets [y | y@(s',_,_,_) <- xs, s' <= sum]
      larger  = sortSubsets [y | y@(s',_,_,_) <- xs, s' > sum]
  in smaller ++ [(sum, sub, i, j)] ++ larger


-- Parent function to calculate the smallest K sets
calcSmallestKSets :: [Int] -> [(Int, [Int], Int, Int)]
calcSmallestKSets numberList =
    let subsets = allContiguousSublists numberList
        subsetTuple = calcSubsetSum subsets
        sortedSubsets = sortSubsets subsetTuple
    in sortedSubsets

-- Recursive print function to print all 4-tuples 
printRows :: [(Int, [Int], Int, Int)] -> IO ()
printRows [] = return ()
printRows ((s, sub, i, j) : xs) = do
    putStrLn $ unwords [ show s, show i, show j, show sub ]
    printRows xs

main :: IO ()
main = do
    let numberList = [24,-11,-34,42,-24,7,-19,21]
    let k = 6
    let kSets = calcSmallestKSets numberList
    -- let size = take k (map (\(x,_,_,_) -> x) kSets)
    -- let sublist = take k (map (\(_,x,_,_) -> x)  kSets)
    -- let indices = take k (map (\(_,_,i,j) -> (i, j))  kSets)
    putStrLn "size i j sublist"
    printRows (take k kSets)





