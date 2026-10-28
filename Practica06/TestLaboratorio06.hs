module Main where

import Laboratorio06

data Test = Test String Bool

passed :: Test -> Bool
passed (Test _ result) = result

renderTest :: Test -> String
renderTest (Test name True) = "OK: " ++ name
renderTest (Test name False) = "ERROR: " ++ name

potencias :: [(Int, Int)]
potencias =
  [(base, exponente) | base <- [-3 .. 3], exponente <- [0 .. 8]]

listas :: [[Int]]
listas =
  [ [],
    [1],
    [1, 2],
    [3, -1, 4],
    [1 .. 20]
  ]

listasDeListas :: [[[Int]]]
listasDeListas =
  [ [],
    [[]],
    [[1, 2]],
    [[1], [], [2, 3]],
    [[1, 2], [3, 4], [5]]
  ]

testPotenciaCola :: Test
testPotenciaCola =
  Test
    "potenciaCola"
    (and
      [ potenciaCola base exponente == potencia base exponente
      | (base, exponente) <- potencias
      ])

testPotenciaCPS :: Test
testPotenciaCPS =
  Test
    "potenciaCPS"
    (and
      [ potenciaCPS base exponente restaSiete
          == restaSiete (potencia base exponente)
      | (base, exponente) <- potencias
      ])
  where
    restaSiete value = value - 7

testLongitudCola :: Test
testLongitudCola =
  Test
    "longitudCola"
    (and [longitudCola xs == longitud xs | xs <- listas])

testLongitudCPS :: Test
testLongitudCPS =
  Test
    "longitudCPS"
    (and [longitudCPS xs doble == doble (longitud xs) | xs <- listas])
  where
    doble value = 2 * value

testReversaCola :: Test
testReversaCola =
  Test
    "reversaCola"
    (and [reversaCola xs == reversa xs | xs <- listas])

testReversaCPS :: Test
testReversaCPS =
  Test
    "reversaCPS"
    (and
      [ reversaCPS xs agregaMarca == agregaMarca (reversa xs)
      | xs <- listas
      ])
  where
    agregaMarca values = 0 : values

testConcatenaCola :: Test
testConcatenaCola =
  Test
    "concatenaCola"
    (and
      [ concatenaCola xss == concatena xss
      | xss <- listasDeListas
      ])

testConcatenaCPS :: Test
testConcatenaCPS =
  Test
    "concatenaCPS"
    (and
      [ concatenaCPS xss agregaMarca == agregaMarca (concatena xss)
      | xss <- listasDeListas
      ])
  where
    agregaMarca values = values ++ [0]

testIntercalaCola :: Test
testIntercalaCola =
  Test
    "intercalaCola"
    (and
      [ intercalaCola xs ys == intercala xs ys
      | xs <- listas,
        ys <- listas
      ])

testIntercalaCPS :: Test
testIntercalaCPS =
  Test
    "intercalaCPS"
    (and
      [ intercalaCPS xs ys agregaMarca == agregaMarca (intercala xs ys)
      | xs <- listas,
        ys <- listas
      ])
  where
    agregaMarca values = values ++ [0]

tests :: [Test]
tests =
  [ testPotenciaCola,
    testPotenciaCPS,
    testLongitudCola,
    testLongitudCPS,
    testReversaCola,
    testReversaCPS,
    testConcatenaCola,
    testConcatenaCPS,
    testIntercalaCola,
    testIntercalaCPS
  ]

report :: String
report = unlines (map renderTest tests)

finish :: Bool -> IO ()
finish True = putStr report
finish False = error report

main :: IO ()
main = finish (all passed tests)
