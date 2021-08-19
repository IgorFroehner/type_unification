import Data.List
import Text.ParserCombinators.Parsec

data Type = TypeInt
    | TypeVar Name
    | TypeArrow Type Type
    deriving Show

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

-- instance Show Type where
--     show TypeInt =
--         "Int"
--     show (TypeVar x) =
--         x
--     show (TypeArrow x y) =
--         "(" ++ show x ++ " -> " ++ show y ++ ")"

occursCheck :: Name -> Type -> Bool
occursCheck n TypeInt = False
occursCheck n (TypeVar m) = n == m
occursCheck n (TypeArrow t1 t2) = 
    occursCheck n t1 || occursCheck n t2 

subst :: Unifier -> Type -> Type
subst u TypeInt = TypeInt
subst u (TypeVar n) = 
    case lookup n u of
        Just e -> e
        Nothing -> TypeVar n
subst u (TypeArrow t1 t2) = TypeArrow (subst u t1) (subst u t2)

unify :: Type -> Type -> Maybe Unifier
unify TypeInt TypeInt = Just []
unify (TypeVar a) (TypeVar b) | a == b = Just []
unify t (TypeVar n) = if occursCheck n t then Just [(n, t)] else Nothing
unify (TypeVar n) t = if occursCheck n t then Just [(n, t)] else Nothing
unify (TypeArrow a b) (TypeArrow x y) = do
    s1 <- unify a x
    s2 <- unify (subst s1 b) (subst s1 y)
    return (compose s2 s1)

substList :: Unifier -> Unifier -

compose :: Unifier -> Unifier -> Unifier
compose [] [] = []
compose [] xs = xs
compose ys [] = ys
compose xs ys = xs ++ substList xs ys

