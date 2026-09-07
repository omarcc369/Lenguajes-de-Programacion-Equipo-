module Interp where

import Grammars
import Data.List (nub, (\\))

-- RETO 3: sustitucion nominal que evita captura

-- Devuelve las variables libres de un identificador.
freeVars :: ASA -> [String]
freeVars (Id x)                                 = [x]
freeVars (Num _)                                = []
freeVars (Boolean _)                            = []
freeVars (Add exprs)                            = concatMap freeVars exprs
freeVars (Sub exprs)                            = concatMap freeVars exprs
freeVars (Mul exprs)                            = concatMap freeVars exprs
freeVars (Div exprs)                            = concatMap freeVars exprs
freeVars (Lt exprs)                             = concatMap freeVars exprs
freeVars (Gt exprs)                             = concatMap freeVars exprs
freeVars (Le exprs)                             = concatMap freeVars exprs
freeVars (Ge exprs)                             = concatMap freeVars exprs
freeVars (And exprs)                            = concatMap freeVars exprs
freeVars (Or exprs)                             = concatMap freeVars exprs
freeVars (Expt expr1 expr2)                     = (freeVars expr1) ++ (freeVars expr2)
freeVars (EqP expr1 expr2)                      = freeVars expr1 ++ freeVars expr2
freeVars (Not expr)                             = freeVars expr
freeVars (Add1 expr)                            = freeVars expr
freeVars (Sub1 expr)                            = freeVars expr
freeVars (ZeroP expr)                           = freeVars expr

-- Colectar variables libres de las ligaduras y quitar del cuerpo las ligadas localmente.
freeVars (Let binds cuerpo)                     = (concatMap (freeVars . snd) binds) ++
                                                    filter (`notElem` (map fst binds)) (freeVars cuerpo)

-- Caso base de LetStar sin ligaduras: las variables libres vienen directamente del cuerpo.
freeVars (LetStar [] cuerpo)                    = freeVars cuerpo
-- Procesar de forma secuencial excluyendo la variable ligada actual del resto de la expresión.
freeVars (LetStar ((lig,exprLig):binds) cuerpo) = (freeVars exprLig) ++
                                                    filter (/= lig) (freeVars (LetStar binds cuerpo))


-- Devuelve el identificador como el único nombre en este nodo.
names :: ASA -> [String]
names (Id x)                 = [x]
names (Num _)                = []
names (Boolean _)            = []
names (Add es)               = concatMap names es
names (Sub es)               = concatMap names es
names (Mul es)               = concatMap names es
names (Div es)               = concatMap names es
names (Lt es)                = concatMap names es
names (Gt es)                = concatMap names es
names (Le es)                = concatMap names es
names (Ge es)                = concatMap names es
names (And es)               = concatMap names es
names (Or es)                = concatMap names es
names (Expt e1 e2)           = names e1 ++ names e2
names (EqP e1 e2)            = names e1 ++ names e2
names (Not e)                = names e
names (Add1 e)               = names e
names (Sub1 e)               = names e
names (ZeroP e)              = names e
names (Let binds cuerpo)     = (map fst binds) ++
                                (concatMap (names . snd) binds) ++
                                names cuerpo
names (LetStar binds cuerpo) = (map fst binds) ++
                                (concatMap (names . snd) binds) ++
                                names cuerpo


-- Genera un nombre fresco que no pertenece a la lista de nombres ocupados.
freshName :: [String] -> String
freshName ns = go (0 :: Int)
  where
    -- Búsqueda recursiva del primer entero `i` tal que `"xi"` sea un nombre nuevo.
    go i =
      let name = "x" ++ show i
      in if name `notElem` ns
           then name
           else go (i + 1)

-- Sustituye la variable 'x' por 'sub' si coincide con el nombre actual 'y'.
sust :: ASA -> String -> ASA -> ASA
sust (Id y) x sub               = if
                                    x == y 
                                then
                                    sub
                                else
                                    Id y
sust (Num n) _ _                = Num n
sust (Boolean b) _ _            = Boolean b
sust (Add exprs) x s            = Add (map (\e -> sust e x s) exprs)
sust (Sub exprs) x s            = Sub (map (\e -> sust e x s) exprs)
sust (Mul exprs) x s            = Mul (map (\e -> sust e x s) exprs)
sust (Div exprs) x s            = Div (map (\e -> sust e x s) exprs)
sust (Lt exprs) x s             = Lt (map (\e -> sust e x s) exprs)
sust (Gt exprs) x s             = Gt (map (\e -> sust e x s) exprs)
sust (Le exprs) x s             = Le (map (\e -> sust e x s) exprs)
sust (Ge exprs) x s             = Ge (map (\e -> sust e x s) exprs)
sust (And exprs) x s            = And (map (\e -> sust e x s) exprs)
sust (Or exprs) x s             = Or (map (\e -> sust e x s) exprs)
sust (Expt expr1 expr2) x s     = Expt (sust expr1 x s) (sust expr2 x s)
sust (EqP expr1 expr2) x s      = EqP (sust expr1 x s) (sust expr2 x s)
sust (Not e) x s                = Not (sust e x s)
sust (Add1 e) x s               = Add1 (sust e x s)
sust (Sub1 e) x s               = Sub1 (sust e x s)
sust (ZeroP e) x s              = ZeroP (sust e x s)

sust (Let binds cuerpo) x s     =
    let (binds', cuerpo') = subindstLet binds cuerpo
        in Let binds' cuerpo'
    where
        fvS = freeVars s
        
        subindstLet binds e

            -- Caso 1: x == y para algún par
            | x `elem` map fst binds =
                -- Solo las expresiones ligadas cambian
                (
                    map (
                        \(
                            ligr,
                            exprLig
                        ) -> (
                            ligr,
                            sust exprLig x s
                        )
                    ) binds ,
                    e
                )

            -- Caso 2: Si x /= y pero alguna variable y está en FV(s)
            | any ( \(y, _) -> y `elem` fvS ) binds =

                let totalNombres = names (Let binds e) ++ names s
                    -- Renombrar las ligaduras en conflicto e ir sustituyendo en el cuerpo
                    (renombreBinds, renombreCuerpo) = 
                        -- Acumula
                        foldl (\(acumulaBinds, acumulaCuerpo) (y, exprLig) ->
                            -- y está en FV(s)
                            if y `elem` fvS then 
                                let z = freshName (totalNombres ++ map fst acumulaBinds)
                                    exprLig' = sust exprLig x s
                                -- Se agrega el sustituyente con la variable fresca y la expr.ligada actualizada,
                                --    & el cuerpo se actualiza a la nueva variable fresca
                                in
                                    (
                                        acumulaBinds ++ [(
                                            z,
                                            exprLig'
                                        )],
                                        sust acumulaCuerpo y (Id z)
                                    )
                            else
                                -- Solo se actualiza la expr.ligada, y el ligador y cuerpo se conservan
                                (
                                    acumulaBinds ++ [(
                                        y,
                                        sust exprLig x s
                                    )],
                                    acumulaCuerpo
                                )
                        ) ([], e) binds
                -- Una vez implementadas las variables frescas, el cuerpo se actualiza a la sustitución
                in (renombreBinds, sust renombreCuerpo x s)
            
            -- Caso 3: x /= & e y no está en FV(s)
            -- Solo se actualizan la exprs.ligadas & el cuerpo, & los ligadores se conservan
            | otherwise =
                (
                    map (\(
                        y,
                        exprLig
                    ) -> (
                        y,
                        sust exprLig x s
                    )) binds,
                    sust e x s
                )

-- Sustituye en LetStar respetando las reglas de alcance secuencial y prevención de captura.
sust (LetStar binds cuerpo) x s =
    let (binds', cuerpo') = subindstLetStar binds cuerpo
        in LetStar binds' cuerpo'
    where
        fvS = freeVars s

        subindstLetStar [] b =
            (
                [],
                sust b x s
            )
        subindstLetStar ((y,exprLig):binds) b
            -- Caso 1: x == y
            -- Sustituye en e1, pero desactiva la sustitución de x en rest y b (shadowing)
            | x == y =
                (
                    (y, sust exprLig x s) : binds ,
                    b
                )

            -- Caso 2: x /= y e y en FV(s)
            -- Renombra y a z: e1[x:=s], rest[y:=z], y e2[y:=z], y luego recursa
            | y `elem` fvS =
                let z = freshName (names (LetStar ((y,exprLig):binds) b) ++ names s)
                    exprLig' = sust exprLig x s
                    binds' = map (\(
                                v,
                                val
                            ) -> (
                                v,
                                sust val y (Id z)
                            )
                        ) binds
                    cuerpo' = sust b y (Id z)
                    LetStar binds'' cuerpo'' = sust (LetStar binds' cuerpo') x s
                in ((z, exprLig') : binds'', cuerpo'')

            -- Caso 3: x /= y e y no está en FV(s)
            -- Sustituye e1[x:=s] y aplica la regla al resto de LetStar
            | otherwise =
                let exprLig' = sust exprLig x s
                    LetStar binds' cuerpo' = sust (LetStar binds b) x s
                in ((y,exprLig'):binds', cuerpo')

-- Caso base cuando no hay más sustituciones por aplicar a la expresión.
sustMany :: ASA -> [Binding] -> ASA
sustMany expr [] = expr
sustMany expr binds =
    let totalNombres =
            names expr ++
            (concatMap (names . snd) binds) ++
            map fst binds
        freshes = foldl (\acc _ -> acc ++ [freshName (totalNombres ++ acc)]) [] binds
        freshPairs = zip binds freshes
        expr' = foldl (\e ((x, _), z) -> sust e x (Id z)) expr freshPairs
        expr'' = foldl (\e ((_, val), z) -> sust e z val) expr' freshPairs
    in expr''


-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.

-- Indicar si una lista contiene elementos duplicados.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

---------------------------- Extractores de valores nativos ----------------------------
toNum :: ASA -> Maybe Int
toNum (Num n) = Just n
toNum _       = Nothing

toBool :: ASA -> Maybe Bool
toBool (Boolean b) = Just b
toBool _           = Nothing
----------------------------------------------------------------------------------------

-- Comprobar recursivamente si una lista cumple la propiedad de orden.
estanOrdenados :: (Int -> Int -> Bool) -> [Int] -> Bool
estanOrdenados _ []          = True
estanOrdenados _ [_]         = True
estanOrdenados operador (x:(y:exprs)) =
    (operador x y)
    &&
    (estanOrdenados operador (y:exprs))

-- Evaluar una lista de expresiones a enteros y verifica que cumplan un operador relacional.
comparaNums :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
comparaNums operador exprs = do
  bExprs <- mapM bigStep exprs
  exprNums <- mapM toNum bExprs
  return (Boolean (estanOrdenados operador exprNums))



-- Evaluación Big-Step de expresiones ASA

bigStep :: ASA -> Maybe ASA
bigStep (Id _)       = Nothing
bigStep (Num n)      = Just (Num n)
bigStep (Boolean b)  = Just (Boolean b)

-- Evaluar todas las subexpresiones y realizar la suma total de sus resultados.
bigStep (Add exprs) = do
    bExprs <- mapM bigStep exprs
    exprNums <- mapM toNum bExprs
    return (Num (sum exprNums))

-- Realizar la resta secuencial de números con codominio en naturales.
bigStep (Sub exprs) = do
    bExprs <- mapM bigStep exprs
    exprNums <- mapM toNum bExprs
    case exprNums of
        []     -> Nothing
        [n]    -> Just (Num 0)
        (n:ns) -> Just (Num (max 0 (n - sum ns)))

-- Evalúa los operandos y calcula su producto total.
bigStep (Mul exprs) = do
    bExprs <- mapM bigStep exprs
    exprNums <- mapM toNum bExprs
    return (Num (product exprNums))

-- Realizar la división comprobando que no exista división entre cero.
bigStep (Div exprs) = do
    bExprs <- mapM bigStep exprs
    exprNums <- mapM toNum bExprs
    case exprNums of
        []     -> Nothing
        [n]    -> if n == 0 then Nothing else Just (Num (1 `div` n))
        (n:ns) -> let p = product ns in if p == 0 then Nothing else Just (Num (n `div` p))

-- Evaluar valores conforme a la operación nativa y a la función auxiliar
bigStep (Lt exprs) = comparaNums (<) exprs
bigStep (Gt exprs) = comparaNums (>) exprs
bigStep (Le exprs) = comparaNums (<=) exprs
bigStep (Ge exprs) = comparaNums (>=) exprs

-- Evaluar una lista de condiciones y regresa True si todas son verdaderas.
bigStep (And exprs) = do
    bEs <- mapM bigStep exprs
    bools <- mapM toBool bEs
    return (Boolean (and bools))

-- Evaluar una lista de condiciones y regresar True si al menos una es verdadera.
bigStep (Or exprs) = do
    bEs <- mapM bigStep exprs
    bools <- mapM toBool bEs
    return (Boolean (or bools))

-- Calcular la potencia entre dos enteros asegurando que el exponente no sea negativo.
bigStep (Expt expr1 expr2) = do
    bExpr1 <- bigStep expr1
    bExpr2 <- bigStep expr2
    exprNum1 <- toNum bExpr1
    exprNum2 <- toNum bExpr2
    if
        exprNum2 >= 0
    then
        Just (Num (exprNum1 ^ exprNum2))
    else
        Nothing

-- Evaluar la igualdad únicamente entre dos expresiones del mismo tipo.
bigStep (EqP expr1 expr2) = do
    bExpr1 <- bigStep expr1
    bExpr2 <- bigStep expr2
    case (bExpr1, bExpr2) of
        (Num exprNum1, Num exprNum2)            -> Just (Boolean (exprNum1 == exprNum2))
        (Boolean exprBool1, Boolean exprBool2)  -> Just (Boolean (exprBool1 == exprBool2))
        _                                       -> Nothing

-- Negar una expresión booleana o evaluar la veracidad de valores numéricos.
bigStep (Not expr) = do
    bExpr <- bigStep expr
    case bExpr of
        Boolean False -> Just (Boolean True)
        Boolean True  -> Just (Boolean False)
        Num _         -> Just (Boolean False)
        _             -> Nothing

-- Evaluar la expresión numérica y sumar uno.
bigStep (Add1 expr) = do
    bExpr <- bigStep expr
    exprNum <- toNum bExpr
    return (Num (exprNum + 1))

-- Evaluar la expresión numérica y restar uno.
bigStep (Sub1 expr) = do
    bExpr <- bigStep expr
    exprNum <- toNum bExpr
    return (Num (max 0 (exprNum - 1)))

-- Determinar si la evaluación de una expresión equivale a cero.
bigStep (ZeroP expr) = do
    bExpr <- bigStep expr
    exprNum <- toNum bExpr
    return (Boolean (exprNum == 0))

-- Evaluar simultáneamente las expresiones en Let sustituirlas en el cuerpo.
bigStep (Let binds cuerpo) =
    if hasDuplicates (map fst binds)
        then
            Nothing
        else
            do
                bindsEval <- mapM (\(lig,exprLig) ->
                        (bigStep exprLig >>= \bExprLig -> Just (lig,bExprLig))
                    ) binds
                bigStep (sustMany cuerpo bindsEval)


bigStep (LetStar [] cuerpo) = bigStep cuerpo

-- Evaluar secuencialmente la primera ligadura y sustituirla en el resto.
bigStep (  LetStar ( (lig,exprLig):binds ) cuerpo  ) = do
    bExprLig <- bigStep exprLig
    bigStep (sust (LetStar binds cuerpo) lig bExprLig)