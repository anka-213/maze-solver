module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    let (from, to, trace) = case args of
            [f,t, tr] -> (f,t, tr) 
            _ -> ("img/big-maze.png", "img/big-maze-solved.png", "img/big-maze-trace")
    -- solveMazeFile from to
    solveTraceMaze from to trace
