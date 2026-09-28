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
    deriving (Eq, Show)

data Value
    = NumV Int
    | BooleanV Bool
    | ClosureV Nombre ASA Env
    deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
        | x `elem` xs = Nothing
        | otherwise   = case (curryFun xs e) of
                Nothing -> Nothing
                Just v  -> Just (Fun x v)

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []  = Nothing
binaryOp _ [_] = Nothing
binaryOp op (x:xs) = Just (foldl op x xs)

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x)                         = Just (Id x)
desugar (NumS n)                        = Just (Num n)
desugar (BooleanS b)                    = Just (Boolean b)

desugar (AddS xs)                       = do
        xs' <- traverse desugar xs
        binaryOp Add xs'

desugar (SubS xs)                       = do
        xs' <- traverse desugar xs
        binaryOp Sub xs'

desugar (NotS e)                        = do
        e' <- desugar e
        return (Not e')

desugar (LetS x exprLig cuerpo)         = do
        exprLig' <- desugar exprLig
        cuerpo'  <- desugar cuerpo
        return (App (Fun x cuerpo') exprLig')

desugar (LetStarS [] _)                 = Nothing
desugar (LetStarS [(x, e)] cuerpo)      = desugar (LetS x e cuerpo)
desugar (LetStarS ((x, e):bs) cuerpo)   = desugar (LetS x e (LetStarS bs cuerpo))

desugar (FunS params cuerpo)            = do
        cuerpo' <- desugar cuerpo
        curryFun params cuerpo'

desugar (AppS funcion args)             = do
        funcion' <- desugar funcion
        args'    <- traverse desugar args
        curryApp funcion' args'


-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((llave, valor):ys)
        | x == llave = Just valor
        | otherwise  = lookupEnv x ys

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x)          = lookupEnv x env
bigStep _   (Num n)         = Just (NumV n)
bigStep _   (Boolean b)     = Just (BooleanV b)

bigStep env (Add i d)       = do
        NumV n <- bigStep env i
        NumV m <- bigStep env d
        return (NumV (n + m))

bigStep env (Sub i d)       = do
        NumV n <- bigStep env i
        NumV m <- bigStep env d
        return (NumV (max 0 (n - m)))

bigStep env (Not e)         = do
        v <- bigStep env e
        case v of
            BooleanV b -> Just (BooleanV (not b))
            NumV _     -> Just (BooleanV False)
            _          -> Nothing

bigStep env (Fun x cuerpo)  = Just (ClosureV x cuerpo env)

bigStep env (App funcion argumento) = do
        cerradura <- bigStep env funcion
        case cerradura of
            ClosureV parametro cuerpo envDef -> do
                valorArg <- bigStep env argumento
                seq valorArg (bigStep ((parametro, valorArg) : envDef) cuerpo)
            _ -> Nothing
