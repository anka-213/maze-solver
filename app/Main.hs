module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    let (from, to) = case args of
            [f,t] -> (f,t) 
            _ -> ("img/big-maze.png", "img/big-maze-solved.png")
    solveMazeFile from to
