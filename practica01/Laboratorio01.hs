module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt ((x ** 2) + (y ** 2))

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares x = sum (map (^ 2) (filter even x))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f (f (f x))

varianza2 :: Double -> Double -> Double
varianza2 v1 v2 =
  let media = (v1 + v2) / 2
  in (((v1 - media)^2) + ((v2 - media)^2)) / 2
 
clasificaTemperatura :: Int -> String
clasificaTemperatura t
  | t <= 0 = "frio extremo"
  | t <= 15 = "frio"
  | t <= 25 = "templado"
  | t <= 35 = "calido"
  | otherwise = "calor extremo"

intercala :: a -> [a] -> [a]
intercala x [] = []
intercala x [y] = [y]
intercala x (y:ys) = y : (x : intercala x ys)

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Suma v1 v2) = (evalua v1) + (evalua v2)
evalua (Producto v1 v2) = (evalua v1) * (evalua v2)