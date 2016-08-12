module Parser 
  (readExpr) where 

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio
import Numeric
import Data.Complex

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
                   Left err -> "No match. " ++ show err
                   Right val -> "Found Val " ++ show val

spaces :: Parser()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             deriving Show

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"rnt"
                  return $ case x of
                             '\\' -> x
                             '"' -> x
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 'f' >> return (Bool False)) <|> (char 't' >> return (Bool True))

parseCharacter :: Parser LispVal
parseCharacter = do
  string "#\\"
  x <- many $ anyChar 
  return $ case x of
             "space" -> Character ' '
             "newline" -> Character '\n'
             _ -> Character (x !! 0)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\"\\")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom $ first:rest

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseHex <|> parseOct <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseDecimalAlt :: Parser LispVal
parseDecimalAlt = do try $ string "#d"
                     x <- many1 digit
                     return $ Number (read x)

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                             bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head $ readFloat (x ++ "." ++ y)) 

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal)
                  sign <- oneOf "+-"
                  y <- (try parseFloat <|> parseDecimal)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseBackquote :: Parser LispVal
parseBackquote = do char '`'
                    x <- parseExpr
                    return $ List [Atom "backquote", x] 

parseCommaList :: Parser LispVal
parseCommaList = do char ','
                    x <- parseExpr
                    return $ List [Atom "comma", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseComplex
         <|> try parseRatio
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> parseBackquote
         <|> parseCommaList
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

