module Eval where

import DataType
import Parser
import Control.Monad
import Control.Monad.Error


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
                  (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op list = fmap (Number . foldl1 op) (mapM unpackNum list)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number x) = return x
unpackNum notNum = throwError $ TypeMismatch "number" notNum

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
