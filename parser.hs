module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
-- import Control.Monad

main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"


spaces :: Parser()
spaces = skipMany1 space 

data LispVal = Atom String 
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many $ escapedChars <|> noneOf "\"\\"
                _ <- char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseBool

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= \x -> (return . Number . read) x

parseDecimal2 :: Parser LispVal
parseDecimal2 = do 
                _ <- try $ string "#d"
                x <- many1 digit
                (return . Number . read) x

hex2dig :: (Num a, Eq a) => String -> a
hex2dig = fst . head . readHex

parseHex :: Parser LispVal
parseHex = do 
            _ <- try $ string "#x"
            x <- many1 hexDigit
            return  $ Number (hex2dig x)

oct2dig :: (Num a, Eq a) => String -> a
oct2dig = fst . head . readOct

parseOct :: Parser LispVal
parseOct = do 
            _ <- try $ string "#o"
            x <- many1 octDigit
            return $ Number (oct2dig x)

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num a => a -> String -> a
bin2dig' d "" = d
bin2dig' d (x:xs) = let
                      n = case x of 
                        '0' -> 0
                        '1' -> 1
                        _ -> error "Fucked"
                      o = 2 * d + n
                    in bin2dig' o xs

parseBin :: Parser LispVal
parseBin = do 
            _ <- try $ string "#b"
            x <- many1 (oneOf "10")
            return $ Number (bin2dig x)

parseBool :: Parser LispVal
parseBool = do
              _ <- char '#'
              (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

escapedChars :: Parser Char 
escapedChars = do 
                  _ <- char '\\' -- a backslash 
                  x <- oneOf "\\\"nrt" -- either backslash or doubleqoute 
                  return $ case x of 
                    '\\' -> x
                    '"' -> x
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _ -> error "Fucked" -- tutorial didn't give a proper behavior for this case... so 












