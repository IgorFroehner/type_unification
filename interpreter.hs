import Data.List
import Text.ParserCombinators.Parsec

data Term = Atom String
          | Variable Name
          | Predicate (String, [Term])

--
-- Nossos construtores:
--   Atom :: String -> Term
--   Variable :: Name -> Term
--   Predicate :: (String, [Term]) -> Term
--

type Name = String

type Unifier = [(Name, Term)]

--
-- atom: lowercase+
--
atom :: Parser Term
atom = do
  -- Extrai um nome com uma ou mais letras minúsculas
  -- Note que:
  --   many1 :: Parser a -> Parser [a]
  --   lower :: Parser Char
  --   many1 lower :: Parser String
  --   name :: String
  name <- many1 lower
  -- Usa o return para colocar o resultado na mônada
  -- Note que:
  --   Atom name :: Term
  return (Atom name)

--
-- variable: uppercase alnum*
--
variable :: Parser Term
variable = do
  -- Compondo sequencialmente!
  --   upper :: Parser Char
  --   head :: Char
  --   many alphaNum :: Parser String
  --   tail :: String
  head <- upper
  tail <- many alphaNum
  -- Agora, retornamos o resultado dentro da mônada
  return (Variable (head:tail))

--
-- predicate: lowercase* "(" list-of-term ")"
--
predicate :: Parser Term
predicate = do
  name <- many1 lower
  char '('
  --
  -- sepBy :: Parser a -> Parser b -> Parser [a]
  --
  subterms <- term `sepBy` char ','
  char ')'
  return (Predicate (name, subterms))

--
-- term: predicate | atom | variable
--
term :: Parser Term
term =
  -- OU um predicado, OU um átomo, OU uma variável...
  --
  -- O motivo do try:
  --   Se eu estiver parseando "x", ele não vai conseguir
  --   ler um predicado, pois não vai encontrar o "(", e
  --   não sabe que deveria ignorar o nome já lido; para
  --   ignorá-lo, usamos o combinador try
  try predicate <|> atom <|> variable

--
-- { X |-> Y }
--
test :: Unifier
test = [
         ("X", Variable "Y")
       ]

main :: IO ()
main = do
  putStrLn "Digite um termo:"
  str <- getLine
  case parse term "<stdin>" str of
    -- Parser com sucesso!
    Right e -> do
      -- print e
      print $ subst test e
    -- Deu ruim!
    Left e ->
      -- Printa o erro...
      print e

--
-- SUBSTITUIÇÃO: recebe um unificador/substituição s
--   e um termo e, e retorna o termo s(e)
--
subst :: Unifier -> Term -> Term

--
-- Átomos fazem parte da estrutura e não mudam; portanto,
--   s(x) = x
--
subst s (Atom x) =
  Atom x

--
--  A substituição, para variáveis, deve verificar se ela
--  existe dentro do unificador
--    s(X) = e      se s contém X |-> e,
--           X      do contrário
--
subst s (Variable x) =
  -- lookup :: Eq a => a -> [(a, b)] -> Maybe b
  -- lookup x s :: Maybe Term
  -- Vamos verificar se X |-> e existe no nosso unificador
  case lookup x s of
    -- Lembrando!
    --   Just :: a -> Maybe a
    --   Nothing :: Maybe a
    Just e ->
      -- Opa, existe!
      -- e :: Term
      e
    Nothing ->
      -- Opa, não existe!
      Variable x

--
-- Para aplicarmos a substituição em um termo composto (predicado),
-- preservamos a estrutura, aplicando recursivamente em subtermos:
--
--    s(x(e1, ..., en)) = x(s(e1), ..., s(en))
-- 
subst s (Predicate (x, es)) =
  -- x :: String
  -- es :: [Term]
  -- Lembrem-se: usamos map pois temos uma LISTA de subtermos!
  Predicate (x, map (subst s) es)

-- Ao invés de usarmos a conversão gerada para string,
-- com o deriving Show, vamos fazer uma na mão!
instance Show Term where
  show (Atom x) =
    x
  show (Variable x) =
    x
  show (Predicate (x, es)) =
    x ++ "(" ++ intercalate "," (map show es) ++ ")"