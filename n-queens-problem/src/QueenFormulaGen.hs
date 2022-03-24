module QueenFormulaGen where

import Picosat ( Solution(Solution) )

type Literal = Int
type Clause = [Literal]
type CNF = [Clause]

-- Get CNF from n
getCNF :: Int -> CNF
getCNF 0             = [[]]
getCNF n | n > 0     = getCNFRows n ++ getCNFTwoCells n
         | otherwise = [[1],[-1]]
 
-- Construct CNF for rows : at least one Queen for each row
getCNFRows :: Int -> CNF
getCNFRows n                          = helper n (n - 1) []
    where helper n i list | i >= 0    = helper n (i - 1) ([ fromPairToSingle n (i, col) | col <- [0..n-1]] : list)
                          | otherwise = list

-- Construct CNF for not beating : it's not true that 2 queens can beat each other
getCNFTwoCells :: Int -> CNF
getCNFTwoCells n = do
                f <- getCells n
                s <- getCells n
                let flit = fromPairToSingle n f
                let slit = fromPairToSingle n s
                if flit < slit && checkCut f s
                    then return [-flit, -slit] -- by De Morgans Law
                    else []

-- Renumbering indexes : from pair to single
fromPairToSingle :: Int -> (Int, Int) -> Int
fromPairToSingle n (i, j) = n * i + j + 1

-- Check if they beat each other
checkCut :: (Int, Int) -> (Int, Int) -> Bool
checkCut (i,j) (l, k) = i == l || j == k || abs (i - l) == abs (j - k)

getCells :: Int -> [(Int, Int)]
getCells n = [ (row, col) | row <- [0..(n - 1)], col <- [0..(n - 1)] ]

-- Renumbering indexes : from single to pair
fromSingleToPair :: Int -> Int -> (Int, Int)
fromSingleToPair n i | n == 0 = (0, 0)
                     | otherwise  = (div (i - 1) n , mod (i - 1) n)