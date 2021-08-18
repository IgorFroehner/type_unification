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
-- whitespace: ' '*
--
whitespace :: Parser ()
whitespace = do
  -- Note que não usamos o many1!
  many (char ' ')
  return ()

--
-- predicate: lowercase* whitespace "(" list-of-term ")"
--
predicate :: Parser Term
predicate = do
  name <- many1 lower
  whitespace
  char '('
  --
  -- sepBy :: Parser a -> Parser b -> Parser [a]
  --
  subterms <- term `sepBy` comma
  char ')'
  return (Predicate (name, subterms))
  where
    -- comma: whitespace "," whitespace
    comma :: Parser ()
    comma = do
      whitespace
      char ','
      whitespace

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
-- unit: term eof
--
unit :: Parser Term
unit = do
  t <- term
  eof
  return t

main :: IO ()
main = do
  putStrLn "Digite um termo:"
  str1 <- getLine
  let Right a = parse unit "<stdin>" str1
  str2 <- getLine
  let Right b = parse unit "<stdin>" str2
  --
  putStrLn "Unificacao:"
  case unify a b of
    Just s ->
      putStrLn $ showUnifier s
    Nothing ->
      putStrLn "Deu ruim!"

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
    x ++ "(" ++ intercalate ", " (map show es) ++ ")"

--
-- Transforma um unificador em string
--
showUnifier [] =
  "{}"
showUnifier xs =
  "{ " ++ intercalate ", " (map showPair xs) ++ " }"
  where
    showPair (x, e) =
      x ++ " |-> " ++ show e

--
-- Queremos verificar se um nome de variável aparece
-- dentro de um termo
--
occursCheck :: Name -> Term -> Bool

--
-- Átomos não tem subtermos e não são variáveis,
-- logo também não contém nenhuma variável!
--
occursCheck x (Atom y) =
  False

--
-- O nome X pode aparecer no termo Y se X == Y!
--
occursCheck x (Variable y) =
  x == y

--
-- Para procurar uma variável X existe no termo
-- x(e1, ..., en), precisamos olhar em cada subelemento!
--
occursCheck x (Predicate (y, es)) =
  -- any :: (a -> Bool) -> [a] -> Bool
  any (occursCheck x) es
  --
  -- Note que, se es = [e1, e2, e3],
  --
  -- Então any (occursCheck x) es =
  --   occursCheck x e1 || occursCheck x e2 || occursCheck x e3
  --

--
--
--
unify :: Term -> Term -> Maybe Unifier

--
-- Regra (REFL):
--
--    ---------------- (REFL)
--       X ~ X = {}
--
unify (Variable x) (Variable y) | x == y =
  -- Existe uma solução, e ela é vazia!
  Just []

--
-- Regra (ATOM):
--
--    --------------- (ATOM)
--       x ~ x = {}
--
unify (Atom x) (Atom y) | x == y =
  -- Existe uma solução vazia!
  Just []

--
-- Regra (LEFT):
--        X isn't free in e
--    ------------------------ (LEFT)
--       X ~ e = { X |-> e }
--
unify (Variable x) e | not (occursCheck x e) =
  Just [(x, e)]

--
-- Regra (RIGHT):
--        X isn't free in e
--    ------------------------ (RIGHT)
--       e ~ X = { X |-> e }
--
unify e (Variable x) | not (occursCheck x e) =
  Just [(x, e)]

--
-- Regra (PRED):
--        [e1, ..., en] ~ [f1, ..., fn] = s
--     ---------------------------------------- (PRED)
--       x(e1, ..., en) ~ x(f1, ..., fn) = s
--
unify (Predicate (x, xs)) (Predicate (y, ys)) | x == y =
  -- Se unifyList retornar Just s, retorna Just s
  -- Se unifyList retornar Nothing, retorna Nothing
  unifyList xs ys

--
-- Caso padrão: se caiu aqui, não bateu com nenhuma
-- equação acima! Logo, por padrão, não unificamos!
--
unify a b =
  Nothing -- :: Maybe a

--
--
--
unifyList :: [Term] -> [Term] -> Maybe Unifier

--
-- Regra (NIL):
--
--     ---------------- (NIL)
--       [] ~ [] = {}
--
unifyList [] [] =
  Just []

--
-- Regra (CONS):
--      x ~ y = s1    s1(xs) ~ s1(ys) = s2
--     ------------------------------------ (CONS)
--          (x:xs) ~ (y:ys) = s2 * s1
--
unifyList (x:xs) (y:ys) = do
  s1 <- unify x y
  s2 <- unifyList (substList s1 xs) (substList s1 ys)
  return (compose s2 s1)
  --
  -- O código acima é equivalente a:
  --
  --   case unify x y of
  --     Just s1 ->
  --       case unifyList (substList s1 xs) (substList s1 ys) of
  --         Just s2 ->
  --           Just (compose s2 s1)
  --         Nothing ->
  --           Nothing
  --     Nothing ->
  --       Nothing
  --
  -- Mas usando mônadas!
  --

--
-- Por padrão, não unifica; vamos cair aqui se o tamanho
-- das listas for diferente!
--
unifyList xs ys =
  Nothing

--
-- Aplica uma substituição em uma lista de termos
--
-- Por exemplo:
--   { X |-> Y }[X, Y, Z] = [Y, Y, Z]
--
substList :: Unifier -> [Term] -> [Term]
substList s xs =
  -- Por exemplo, se xs = [a, b, c], temos que
  --   map (subst s) xs = [subst s a, subst s b, subst s c]
  map (subst s) xs

--
-- Aplica uma substituição em um unificador
--
substUnifier :: Unifier -> Unifier -> Unifier
substUnifier s xs =
  --
  -- Por exemplo, aplicando { X |-> Y } em
  --   { A |-> foo(X, X) } nós teremos 
  --   { A |-> foo(Y, Y) }
  --
  map (\(x, e) -> (x, subst s e)) xs

--
-- Compõe duas substituições
--
compose :: Unifier -> Unifier -> Unifier
compose s2 s1 =
  s2 ++ substUnifier s2 s1