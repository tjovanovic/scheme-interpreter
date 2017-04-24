module Main where

import System.IO
import System.Environment
import DataType
import Parser
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr >>= eval)

readExpr :: String -> ThrowsError LispVal
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
