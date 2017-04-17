module Main where

import System.Environment
import Parser
import Eval

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
