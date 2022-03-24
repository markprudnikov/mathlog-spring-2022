module Main where

import QueenFormulaGen
import Picosat ( solve, Solution(Solution) )
import System.Environment (getArgs)

replace :: Int -> a -> [a] -> [a]
replace index newVal list = take index list ++ newVal : drop (index + 1) list

setQueen :: (Int, Int) -> [[Char]] -> [[Char]]
setQueen (x,y) pole = replace x (replace y 'Q' (pole !! x)) pole

setAllQueens :: Int -> Solution -> [[Char]] -> [[Char]]
setAllQueens n (Solution []) pole = pole
setAllQueens n (Solution (x:xs)) pole | x < 0 = setAllQueens n (Solution xs) pole
                                      | otherwise = let p = fromSingleToPair n x in setAllQueens n (Solution xs) $ setQueen p pole
setAllQueens n _ _ = ["Problem has no solutions"]

setToString :: [[Char]] -> String
setToString ["Problem has no solutions"] = "Problem has no solutions"
setToString lst = "----------\nSolution:\n----------\n" ++ foldr (\w -> (("[" ++ w ++ "]\n") ++)) [] lst ++ "----------"

main :: IO ()
main = do
    input <- getArgs
    let n = read $ head input :: Int
    let pole = ([ replicate n x | x <- replicate n '.'])
    solution <- solve (getCNF n)
    putStrLn (setToString (setAllQueens n solution pole))
