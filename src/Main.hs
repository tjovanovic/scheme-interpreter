module Main where

import System.IO
import System.Environment
import DataType
import Parser
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ legit prompt action = do
  result <- prompt
  unless (legit result) $ action result >> until_ legit prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp >>> ") evalAndPrint


main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint (head args)
    _ -> putStrLn "Wrong input bro"
