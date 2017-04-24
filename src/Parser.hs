module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import DataType

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $
        case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit
-- parseNumber = many1 digit >>= \x -> return (Number (read x))

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy  parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x

