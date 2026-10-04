module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.


curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | otherwise = curryFun xs e >>= \v -> Just (Fun x v)


curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []  = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:xs) = Just (foldl op x xs)

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] ramaElse = desugar ramaElse
desugarCond ((condicion, rama) : clausulas) ramaElse =
  If <$> desugar condicion
     <*> desugar rama
     <*> desugarCond clausulas ramaElse

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") [FunS [f] definicion]) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)

desugar (AddS xs) =
  traverse desugar xs >>= \xs' ->
    binaryOp Add xs'

desugar (SubS xs) =
  traverse desugar xs >>= \xs' ->
    binaryOp Sub xs'

desugar (NotS e) =
  desugar e >>= \e' ->
    return (Not e')

desugar (LetS x exprLig cuerpo) =
  desugar exprLig >>= \exprLig' ->
    desugar cuerpo >>= \cuerpo' ->
      return (App (Fun x cuerpo') exprLig')

desugar (LetStarS [] _) = Nothing
desugar (LetStarS [(x, e)] cuerpo) = desugar (LetS x e cuerpo)
desugar (LetStarS ((x, e):bs) cuerpo) =
  desugar (LetS x e (LetStarS bs cuerpo))

desugar (FunS params cuerpo) =
  desugar cuerpo >>= \cuerpo' ->
    curryFun params cuerpo'

desugar (AppS funcion args) =
  desugar funcion >>= \funcion' ->
    traverse desugar args >>= \args' ->
      curryApp funcion' args'

desugar (IfS condicion consecuente ramaElse) =
  If <$> desugar condicion <*> desugar consecuente <*> desugar ramaElse
desugar (CondS clausulas ramaElse) = desugarCond clausulas ramaElse
desugar (LetRecS nombre definicion cuerpo) =
  desugar (LetS nombre (AppS (IdS "Y") [FunS [nombre] definicion]) cuerpo)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((y,v): resto)
  | x == y = Just v
  | otherwise = lookupEnv x resto

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (ExprV e env) = bigStep env e >>= strict
strict v = Just v

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num v) = Just (NumV v)
bigStep _ (Boolean v) = Just (BooleanV v )
bigStep env (Id v) = lookupEnv v env
bigStep env (Fun x body) = Just (ClosureV x body env)
bigStep env (Add i d) =
  evalStrict env i >>= \i' ->
    evalStrict env d >>= \d' ->
      sumaV i' d'
bigStep env (Sub i d) =
  evalStrict env i >>= \i' ->
    evalStrict env d >>= \d' ->
      restaV i' d'
bigStep env (Not e) =
  evalStrict env e >>= notV
bigStep env (If c t e) =
  evalStrict env c >>= \c' -> chooseBranch env t e c'
bigStep env (App f a) =
  evalStrict env f >>= \f' -> apply env a f'

evalStrict :: Env -> ASA -> Maybe Value
evalStrict env e = bigStep env e >>= strict

sumaV :: Value -> Value -> Maybe Value
sumaV (NumV a) (NumV b) = Just (NumV (a + b))
sumaV _ _ = Nothing

restaV :: Value -> Value -> Maybe Value
restaV (NumV a) (NumV b) = Just (NumV (max 0 (a - b)))
restaV _ _ = Nothing

notV :: Value -> Maybe Value
notV (BooleanV b) = Just (BooleanV (not b))
notV _ = Nothing

chooseBranch :: Env -> ASA -> ASA -> Value -> Maybe Value
chooseBranch env t _ (BooleanV True) = bigStep env t
chooseBranch env _ e (BooleanV False) = bigStep env e
chooseBranch _ _ _ _ = Nothing

apply :: Env -> ASA -> Value -> Maybe Value
apply env a (ClosureV p b env') = bigStep ((p, ExprV a env) : env') b
apply _ _ _ = Nothing
