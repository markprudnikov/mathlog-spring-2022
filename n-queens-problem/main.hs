import Picosat ( solve, Solution(Solution) )

type Literal = Int
type Clause = [Literal]
type CNF = [Clause]

-- Check if they are in the same col / row / diag
checkCut :: (Int, Int) -> (Int, Int) -> Bool
checkCut (i,j) (l, k) = i == l || j == k || abs (i - l) == abs (j - k)

-- From pair to line
fromPairToSingle :: Int -> (Int, Int) -> Int
fromPairToSingle n (i, j) = n * i + j + 1

-- From line to pair
fromSingleToPair :: Int -> Int -> (Int, Int)
fromSingleToPair n i = (div (i - 1) n , mod (i - 1) n)

-- get cnf from n
getCNF :: Int -> CNF
getCNF 0 = [[]]
getCNF n | n > 0 = getCNF' n [ (row, col) | row <- [0..(n - 1)], col <- [0..(n - 1)] ]
         | otherwise = [[1],[-1]]

getCNF' :: Int -> [(Int, Int)] -> CNF
getCNF' n cells = getCNFRows n ++ getCNFTwoCells n cells

getCNFRows :: Int -> CNF -- each row at least 1 queen 
getCNFRows n = helper n (n-1) []
    where helper n i list | i >= 0    = helper n (i - 1) ([ fromPairToSingle n (i, col) | col <- [0..n-1]] : list)
                          | otherwise = list

getCNFTwoCells :: Int -> [(Int,Int)] -> CNF -- it's not true that 2 queens can beat each other
getCNFTwoCells n cells = do
                f <- cells
                s <- cells
                let flit = fromPairToSingle n f
                let slit = fromPairToSingle n s
                if flit < slit && checkCut f s
                    then return [-flit, -slit] -- De Morgans Law
                    else []

resolveSAT :: Int -> CNF -> IO String
resolveSAT n cnf = fmap (printResult n) (solve cnf)

printResult :: Int -> Solution -> String
printResult n (Solution []) = ""
printResult n (Solution (x:xs)) | x < 0 = printResult n (Solution xs)
                                | otherwise = let p = fromSingleToPair n x in show (fst p + 1) ++ " " ++ show (snd p + 1) ++ "; " ++ printResult n (Solution xs)
printResult n _ = " "

getQueensSet :: Int -> IO String
getQueensSet n = resolveSAT n (getCNF n)