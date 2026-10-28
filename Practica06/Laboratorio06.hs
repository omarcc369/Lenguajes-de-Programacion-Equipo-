module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola x n = potenciaCola' x n 1

potenciaCola' :: Int -> Int -> Int -> Int
potenciaCola' _ 0 acc = acc
potenciaCola' x n acc = potenciaCola' x (n-1) (acc*x)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS x n k = potenciaCPS x (n-1) (\v -> k (v*x))

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola xs = longitudCola' xs 0

longitudCola' :: [a] -> Int -> Int
longitudCola' [] acc = acc
longitudCola' (x:xs) acc = longitudCola' xs (acc+1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k = k 0
longitudCPS (x:xs) k = longitudCPS xs (\v -> k (v+1))

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = reversaCola' xs []

reversaCola' :: [a] -> [a] -> [a]
reversaCola' [] acc = acc
reversaCola' (x:xs) acc = reversaCola' xs (x:acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k(v++[x]))

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xs = concatenaCola' xs []

concatenaCola' :: [[a]] -> [a] -> [a]
concatenaCola' [] acc = acc
concatenaCola' (x:xs) acc = concatenaCola' xs (acc++x)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k = k []
concatenaCPS (x:xs) k = concatenaCPS xs (\v -> k(x++v))

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = intercalaCola' xs ys []

intercalaCola' :: [a] -> [a] -> [a] -> [a]
intercalaCola' [] ys acc = acc ++ ys
intercalaCola' xs [] acc = acc ++ xs
intercalaCola' (x:xs) (y:ys) acc = intercalaCola' xs ys (acc++[x,y])

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x:xs) (y:ys) k = intercalaCPS xs ys (\v -> k ([x,y]++v))
