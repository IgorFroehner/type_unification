import Data.List
import Text.ParserCombinators.Parsec

data Type = TypeInt
    | TypeVar Name
    | TypeArrow Type Type
    -- deriving Show

type Name = String

type Unifier = [(Name, Type)]

whitespace :: Parser ()
whitespace = do
  many (char ' ')
  return ()

arrow :: Parser()
arrow = do
    whitespace
    char '-'
    char '>'
    whitespace

parseAtom :: Parser Type -- atom: int | var | paren
parseAtom =
    try parseInt <|> parseVar <|> parseParen

parseInt :: Parser Type -- int: "Int"
parseInt = do
    char 'I'
    char 'n'
    char 't'
    return TypeInt

parseVar :: Parser Type -- var: lowercase+
parseVar = do
    name <- many1 lower
    return (TypeVar name)

parseFun :: Parser Type -- fun: atom "->" type
parseFun = do
    atom1 <- parseAtom
    arrow
    TypeArrow atom1 <$> parseType

parseParen :: Parser Type -- paren: "(" type ")"
parseParen = do
    whitespace
    char '('
    whitespace
    asdf <- parseType
    whitespace
    char ')'
    whitespace
    return asdf

parseType :: Parser Type -- type: function | atom
parseType =
    try parseFun <|> parseAtom

unit :: Parser Type
unit = do
    t <- parseType
    eof
    return t

main :: IO()
main = do
    putStrLn "Digite os tipos:"
    str <- getLine

    let res = parse unit "<stdin>" str

    case res of
        Right s -> print s
        Left _ -> putStrLn "Deu ruim"

instance Show Type where
    show TypeInt =
        "Int"
    show (TypeVar x) =
        x
    show (TypeArrow x y) =
        "(" ++ show x ++ " -> " ++ show y ++ ")"
