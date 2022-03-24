module Main where

import Mylib ( getCNF, printResult )
import Picosat ( solve, Solution(Solution) )
import System.Environment (getArgs)

main :: IO ()
main = do
    input <- getArgs
    let n = read $ head input :: Int
    res <- solve (getCNF n)
    case res of (Solution s) -> putStrLn (printResult n res)
                _ -> putStrLn "UNSAT"
