module Interp where

import Grammars
import Data.Maybe (Maybe(Nothing))

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
desugar e = case e of
    IdS x          -> IdS x
    NumS n         -> Num n
    BooleanS b     -> Boolean b
    Add xs
        | Just xs' <- transverse desugar xs = binaryOp Add xs
        | otherwise = Nothing
-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x)                         = Just (Id x)
desugar (NumS n)                        = Just (Num n)
desugar (BooleanS b)                    = Just (Boolean b)

desugar (AddS xs)                       = do
        | Just xs' <- traverse desugar xs = binaryOp Add xs'
        | otherwise                       = Nothing

desugar (SubS xs)                       = do
        | Just xs' <- traverse desugar xs = binaryOp Sub xs'
        | otherwise                       = Nothing

desugar (NotS e)                        = do
        | Just e'      <- desugar e = (Not e')
        | otherwise                 = Nothing

desugar (LetS x exprLig cuerpo)         = do
        exprLig'     <- desugar exprLig
        cuerpo' <- desugar cuerpo
    return (App (Fun x cuerpo') exprLig')

desugar (LetS x exprLig cuerpo)         = do
        cuerpo' <- desugar cuerpo
        Just (App (Fun x cuerpo') exprLig')
    
desugar (AppS funcion arg)              = do
        funcion'        <- desugar funcion
        arg'            <- mapM desugar arg
        curryApp funcion' arg'

desugar (LetStarS [] cuerpo)            = desugar cuerpo
desugar (LetStarS ((x e):bs) cuerpo)    = App (Fun x desugar(LetStarS bs cuerpo)) (desugar e)


-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
            lookupEnv x [] = Nothing
            lookupEnv x ((llave,valor):ys)
                | x == llave    = Just valor
                | ottherwise    = lookupEnv x ys
            

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
