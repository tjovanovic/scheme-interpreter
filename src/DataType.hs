module DataType where

import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool


showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList contents end) = "(" ++ unwordsList contents ++ " . " ++ showVal end ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default


type ThrowsError = Either LispError
