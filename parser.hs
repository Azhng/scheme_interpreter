{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Ratio
import Data.Array
import Data.Complex
import Control.Monad
import Control.Monad.Error 
import System.IO 
import Data.IORef

main :: IO()
main = do 
        args <- getArgs
        case length args of 
          0 -> runRepl
          1 -> runOne $ args !! 0
          otherwise -> putStrLn "Program takes only 0 or 1 argument"

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

-- eval :: Env -> LispVal -> IOThrowError LispVal
-- eval env val@(String _) = return val
-- eval env val@(Number _) = return val
-- eval env val@(Bool _) = return val
-- eval env (Atom id) = getVar env id
-- eval env (List [Atom "quote", val]) = return val
-- eval env (List [Atom "if", pred, conseq, alt]) = do 
--                                                   result <- eval env pred
--                                                   case result of
--                                                     Bool False -> eval env alt
--                                                     otherwise -> eval env conseq
-- eval env (List [Atom "set!", Atom var, form]) =
--      eval env form >>= setVar env var
-- eval env (List [Atom "define", Atom var, form]) =
--      eval env form >>= defineVar env var
-- eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
-- eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

eval :: Env -> LispVal -> IOThrowError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val 
eval env val@(Bool _) = return val 
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", prede, conseq, alt]) = do 
                                                result <- eval env prede
                                                case result of 
                                                  Bool False -> eval env alt 
                                                  otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
              ("list?", unaryOp listp),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


boolBinop :: (LispVal -> ThrowError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowError LispVal
boolBinop unpacker op args = if length args /= 2
                              then throwError $ NumArgs 2 args 
                              else do left <- unpacker $ args !! 0
                                      right <- unpacker $ args !! 1
                                      return $ Bool $ left `op` right 

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowError LispVal
strBoolBinop = boolBinop unpackStr  

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowError LispVal
boolBoolBinop = boolBinop unpackBool 



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

unpackStr :: LispVal -> ThrowError String 
unpackStr (String s) = return s 
unpackStr (Number s) = return $ show s 
unpackStr (Bool s)   = return $ show s 
unpackStr noString   = throwError $ TypeMismatch "string" noString


unpackBool :: LispVal -> ThrowError Bool 
unpackBool (Bool b) = return b 
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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



-- more lisp primitives 
car :: [LispVal] -> ThrowError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg 
car badArgList              = throwError $ NumArgs 1 badArgList 


cdr :: [LispVal] -> ThrowError LispVal
cdr [List (x : xs)]         = return $ List xs 
cdr [DottedList [_] x]      = return x 
cdr [DottedList (_ : xs) x] = return $ DottedList xs x 
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg 
cdr badArgList              = throwError $ NumArgs 1 badArgList 

cons :: [LispVal] -> ThrowError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs 
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList 

eqv :: [LispVal] -> ThrowError LispVal
eqv [(Bool arg1), (Bool arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2 
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2 
eqv [(Atom arg1), (Atom arg2)]     = return $ Bool $ arg1 == arg2 
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]] 
eqv [(List arg1), (List arg2)]     = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
                                        where eqvPair (x1, x2) = case eqv [x1, x2] of 
                                                                  Left err -> False 
                                                                  Right (Bool val) -> val 
eqv [_, _]                         = return $ Bool False 
eqv badArgList                     = throwError $ NumArgs 2 badArgList          


-- Weak Typing 
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do 
                                                  unpacked1 <- unpacker arg1
                                                  unpacked2 <- unpacker arg2
                                                  return $ unpacked1 == unpacked2
                                                  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowError LispVal
equal [arg1, arg2] = do 
                      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                                          [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                      eqvEquals <- eqv [arg1, arg2]
                      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


-- Building REPL 

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String 
evalString env expr = runIOThrow $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m() 
until_ pred prompt action = do
                            result <- prompt
                            if pred result 
                              then return()
                              else action result >> until_ pred prompt action

runOne :: String -> IO()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO()
runRepl = nullEnv >>= until_ (=="quit") (readPrompt "Lisp>> ") . evalAndPrint

-- Storing variable 

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef [] 

type IOThrowError = ErrorT LispError IO 

liftThrows :: ThrowError a -> IOThrowError a 
liftThrows (Left err) = throwError err 
liftThrows (Right val) = return val 

runIOThrow :: IOThrowError String -> IO String 
runIOThrow action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool 
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var 

getVar :: Env -> String -> IOThrowError LispVal
getVar envRef var = do 
                      env <- liftIO $ readIORef envRef
                      maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                            (liftIO . readIORef)
                            (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowError LispVal
setVar envRef var value = do 
                            env <- liftIO $ readIORef envRef 
                            maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                  (liftIO . (flip writeIORef value))
                                  (lookup var env)
                            return value

defineVar :: Env -> String -> LispVal -> IOThrowError LispVal
defineVar envRef var value = do 
                              alreadyDefined <- liftIO $ isBound envRef var 
                              if alreadyDefined 
                                then setVar envRef var value >> return value 
                                else liftIO $ do 
                                                valueRef <- newIORef value 
                                                env <- readIORef envRef
                                                writeIORef envRef ((var, valueRef) : env)
                                                return value

bindVars :: Env -> [(String, LispVal)] -> IO Env 
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef 
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value 
                                        return (var, ref)












