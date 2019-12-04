module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [from,to, trace] -> solveTraceMaze from to trace
        [from,to] -> solveMazeFile from to
        _ -> solveMazeFile "img/big-maze.png" "img/big-maze-solved.png"
        -- _ -> ("img/big-maze.png", "img/big-maze-solved.png", "img/big-maze-trace")
    -- solveMazeFile from to
    -- solveTraceMaze from to trace
