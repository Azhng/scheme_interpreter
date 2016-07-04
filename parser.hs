module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Ratio
import Data.Array
import Data.Complex
import Control.Monad
import Control.Monad.Error 

main :: IO()
main = do 
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled

symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowError LispVal 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> throwError $ Parser err
    Right val -> return val


spaces :: Parser()
spaces = skipMany1 space 

data LispVal = Atom String 
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | Float Double
              | String String
              | Bool Bool
              | Ratio Rational
              | Complex (Complex Double)
              | Vector (Array Int LispVal)


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
parseNumber = try parseComplex
          <|> try parseFloat
          <|> try parseRatio
          <|> try parseDecimal1 
          <|> try parseDecimal2 
          <|> try parseHex 
          <|> try parseOct 
          <|> try parseBin

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseBool
          <|> parseQuoted
          <|> parseQuasiQuoted
          <|> parseUnQuote
          <|> parseVector
          <|> do 
                  _ <- char '('
                  x <- try parseLists <|> parseDottedLists
                  _ <- char ')'
                  return x

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

parseFloat :: Parser LispVal
parseFloat = do
              x <- many1 digit
              _ <- char '.'
              y <- many1 digit
              return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do 
              x <- many1 digit
              _ <- char '/'
              y <- many1 digit
              return $ Ratio $ (read x) % (read y)

toDouble :: LispVal -> Double 
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n
toDouble _ = error "wrong type " 



parseComplex :: Parser LispVal
parseComplex = do
                x <- (try parseFloat <|> try parseDecimal1)
                _ <- char '+'
                y <- (try parseFloat <|> try parseDecimal1)
                _ <- char 'i'
                return $ Complex $ toDouble x :+ toDouble y

parseLists :: Parser LispVal
parseLists = liftM List $ sepBy parseExpr spaces 

parseDottedLists :: Parser LispVal
parseDottedLists = do 
                    head <- endBy parseExpr spaces 
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail 

parseQuoted :: Parser LispVal
parseQuoted = do
                _ <- char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do 
                    _ <- char '\''
                    x <- parseExpr
                    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
                _ <- char '\''
                x <- parseExpr
                return $ List [Atom "unquote", x] 

parseVector :: Parser LispVal
parseVector = try $ do 
                _ <- string "#("
                v <- sepBy parseExpr spaces 
                _ <- char ')'
                return $ Vector $ listArray (0, (length v - 1)) v

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




-- Eval 

showVal :: LispVal -> String 
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name 
showVal (Number contents) = show contents 
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal _ = "fucked"

unwordsList :: [LispVal] -> String 
unwordsList = unwords . map showVal


instance Show LispVal where show = showVal

eval :: LispVal -> ThrowError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val 
eval val@(Bool _) = return val 
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowError LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


primitives :: [(String, [LispVal] -> ThrowError LispVal)]
primitives = [("+", numericBinop (+)), 
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowError LispVal
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params 
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowError LispVal
unaryOp f [v] = return $ f v 
unaryOp _ [] = throwError $ NumArgs 1 [] 
unaryOp _ _ = throwError $ Default "Incorrect application of unary operator"

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _        = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp (Bool _)     = Bool True
boolp _            = Bool False
listp (List _)       = Bool True 
listp (DottedList _ _) = Bool True
listp _              = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _        = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

unpackNum :: LispVal -> ThrowError Integer 
unpackNum (Number n) = return n 
unpackNum (String n) = let parsed = reads n in 
                        if null parsed 
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum



-- Error Handling 

data LispError = NumArgs Integer [LispVal]
                  | TypeMismatch String LispVal
                  | Parser ParseError
                  | BadSpecialForm String LispVal
                  | NotFunction String String 
                  | UnboundVar String String  
                  | Default String


showError :: LispError -> String 
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form 
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type; expected " ++ expected ++ ", found " ++ show found 
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where 
                            noMsg = Default "An error has occurred"
                            strMsg = Default

type ThrowError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String 
trapError action = catchError action (return . show)

extractValue :: ThrowError a -> a
extractValue (Right val) = val





