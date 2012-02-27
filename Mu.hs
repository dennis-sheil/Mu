import Data.List (intercalate, findIndex)
import Data.Maybe (fromJust)

import qualified System as S

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L
import Text.ParserCombinators.Parsec ((<|>))

import qualified Control.Monad as M


-- The data definitions for representing functions.

type NamedFunction = (String, Function)
data Function = Zero | Succ | Pi Int Int | Chi Function [Function] | Rho Function Function | Mu Function
    deriving Show
type Value = (String, Function, [Integer])


-- Functions for checking functions and evaluating them.

-- Returns the number of arguments a function expects.
arity :: Function -> Int
arity (Pi n _) = n
arity (Chi _ g) = arity $ g !! 0
arity (Rho g _) = arity g + 1
arity (Mu f) = arity f - 1
arity _ = 1

-- Checks that the function is valid.
check :: Function -> Bool
check (Pi n k) = n >= 1 && 1 <= k && k <= n
check (Chi f gs) = (arity f == length gs) && and (map (== (arity $ gs !! 0)) $ map arity gs)
check (Rho g h) = arity g + 2 == arity h
check (Mu f) = arity f >= 1
check _ = True

-- Evaluates the function on the input arguments.
eval :: Function -> [Integer] -> Integer
eval f args | arity f /= length args || not (check f) = error "Invalid request"
eval Zero _ = 0
eval Succ [x] = x + 1
eval (Pi _ k) args = args !! (k - 1)
eval (Chi f gs) args = eval f $ map ((flip eval) args) gs
eval f@(Rho g h) (x:xs) = case x of
    0 -> eval g xs
    _ -> eval h ((x-1):(eval f ((x-1):xs)):xs)
eval (Mu f) args = toInteger $ fromJust $ findIndex (== 0) $ map g [0..]
    where
        g i = eval f ((toInteger i):args)
eval _ _ = error "Something really weird just happened!"

-- UGLY CODE BELOW: lexing and parsing code files

type Lookup = [NamedFunction]

lexer :: T.TokenParser ()
lexer = T.makeTokenParser (L.emptyDef)

identifier = T.identifier lexer
symbol = T.symbol lexer
natural = T.natural lexer
semi = T.semi lexer
commaSep = T.commaSep lexer
parens = T.parens lexer
brackets = T.brackets lexer

parseProgram :: [String] -> [Value]
parseProgram lis = parseProgram' lis [("zero", Zero), ("succ", Succ)] []
    where
        parseProgram' [] _ acc = acc
        parseProgram' (l:ls) defns acc = case (P.parse (progLine defns) "" l) of
            Right (vs, defns') -> parseProgram' ls defns' (acc ++ vs)
            Left _ -> error $ "Failed to parse line: " ++ l

progLine :: Lookup -> P.Parser ([Value], Lookup)
progLine l = commentLine <|> P.try funcLine <|> valLine
    where
        funcLine = do
            f <- func l
            return ([], f:l)
        valLine = do
            v <- val l
            return ([v], l)
        commentLine = do
            symbol "%"
            return ([], l)

val :: Lookup -> P.Parser Value
val dict = do
    i <- identifier
    args <- parens (commaSep natural)
    return (i, fromJust $ lookup i dict, args)

func :: Lookup -> P.Parser NamedFunction
func dict = do
    i <- identifier
    _ <- symbol "="
    b <- funcBody dict
    return (i, b)

funcBody :: Lookup -> P.Parser Function
funcBody dict = zero <|> suc <|> p <|> chi <|> rho <|> mu
    where
        zero = symbol "zero" >> return Zero
        suc = symbol "succ" >> return Succ
        p = do
            _ <- symbol "pi"
            _ <- symbol "["
            n <- natural
            _ <- semi
            k <- natural
            _ <- symbol "]"
            return $ Pi (fromIntegral n) (fromIntegral k)
        chi = do
            _ <- symbol "chi"
            _ <- symbol "["
            f' <- identifier
            _ <- semi
            gs' <- commaSep identifier
            _ <- symbol "]"
            let f = fromJust $ lookup f' dict
            let gs = map (fromJust . (flip lookup) dict) gs'
            return $ Chi f gs
        rho = do
            _ <- symbol "rho"
            _ <- symbol "["
            f' <- identifier
            _ <- semi
            g' <- identifier
            _ <- symbol "]"
            let f = fromJust $ lookup f' dict
            let g = fromJust $ lookup g' dict
            return $ Rho f g
        mu = do
            _ <- symbol "mu"
            f' <- brackets identifier
            let f = fromJust $ lookup f' dict
            return $ Mu f

-- Input/Output of the interpreter

getLines :: FilePath -> IO [String]
getLines = M.liftM lines . readFile

main :: IO ()
main = do
    args <- S.getArgs
    let fileName = args !! 0
    ls <- getLines fileName
    let values = parseProgram ls
    _ <- M.mapM outputVal values
    return ()

outputVal :: Value -> IO ()
outputVal (n, f, args) = do
    putStrLn $ n ++ "(" ++ (intercalate "," $ map show args) ++ ") = " ++ (show $ eval f args)
