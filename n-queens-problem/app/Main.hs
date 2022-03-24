module Main where

import QueenFormulaGen ( getCNF, fromSingleToPair )
import Picosat ( solve, Solution(Solution) )
import System.Environment (getArgs)

solutionToString :: Int -> Solution -> String
solutionToString _ (Solution []) = ""
solutionToString n (Solution (x:xs)) | x < 0 = solutionToString n (Solution xs)
                                     | otherwise = let p = fromSingleToPair n x in "\n" ++ show (fst p + 1) ++ " " ++ show (snd p + 1) ++ solutionToString n (Solution xs)
solutionToString _ _ = "\nProblem has no solution"

main :: IO ()
main = do
    input <- getArgs
    let n = read $ head input :: Int
    solution <- solve (getCNF n)
    putStrLn ("----------\nSolution:\n----------" ++ solutionToString n solution ++ "\n----------")
