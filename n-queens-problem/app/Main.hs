module Main where

import QueenFormulaGen ( getCNF, fromSingleToPair )
import Picosat ( solve, Solution(Solution) )
import System.Environment (getArgs)

replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

setQueen :: (Int, Int) -> [[Char]] -> [[Char]]
setQueen (x,y) pole = replace x (replace y 'Q' (pole !! x)) pole

setAllQueens :: Int -> Solution -> [[Char]] -> [[Char]]
setAllQueens n (Solution []) pole = pole
setAllQueens n (Solution (x:xs)) pole | x < 0 = setAllQueens n (Solution xs) pole
                                      | otherwise = let p = fromSingleToPair n x in setAllQueens n (Solution xs) $ setQueen p pole
setAllQueens n _ _ = ["Problem has no solution"]

setToString :: [[Char]] -> String
setToString ["Problem has no solution"] = "Problem has no solution"
setToString lst = "----------\nSolution:\n----------\n" ++ foldr (\slovo -> (("[" ++ slovo ++ "]\n") ++)) [] lst ++ "----------"

main :: IO ()
main = do
    input <- getArgs
    let n = read $ head input :: Int
    let pole = ([ replicate n x | x <- replicate n '.'])
    solution <- solve (getCNF n)
    putStrLn (setToString (setAllQueens n solution pole))
