module Main where

{-- Compilado 02: Consignas Omega --}

-- 1. Escribir una función que sume dos números enteros.

suma:: Int -> Int -> Int
suma x y = x + y

-- 2. Implementar una función que calcule el área de un círculo dado su radio.

radio:: Double -> Double
radio x = pi * x * x

-- 3. Definir una función que determine si un número es par o impar.

espar:: Int -> Bool
espar x = x `mod` 2 == 0

-- 4. Escribir una función que calcule el factorial de un número.

factorial:: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- 5. Implementar una función que invierta una lista.

invertirLista:: [a] -> [a]
invertirLista [] = []
invertirLista(x:xs) = invertirLista xs ++ [x]

-- 6. Definir una función que determine si una lista está ordenada de forma ascendente.

esOrdenAsc:: [Int] -> Bool
esOrdenAsc [] = False
esOrdenAsc [x] = True
esOrdenAsc (x:y:xs) = x <= y && esOrdenAsc (y : xs)

-- 7. Escribir una función que cuente la cantidad de elementos en una lista.

cantElementos:: [a] -> Int
cantElementos [] = 0
cantElementos (x:xs) = 1 + cantElementos xs

-- 8. Implementar una función que obtenga los elementos en posiciones pares de una lista.

elemPares:: [Int] -> [Int]
elemPares [] = []
elemPares (x:xs) = if even x then x : elemPares xs else elemPares xs

-- 9. Definir una función que calcule el máximo común divisor de dos números.

mcd :: Int -> Int -> Int
mcd x 0 = x
mcd x y = mcd y (mod x y) -- Euclides: el MCD de dos números x e y es igual al MCD de y, y el residuo de la división de x entre y.

-- 10. Implementar una función que calcule la suma de los dígitos de un número entero.

sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos x = x `mod` 10 + sumaDigitos (x `div` 10)

-- 11. Definir una función que encuentre el elemento mínimo en una lista.

menor:: Int -> Int -> Int
menor x y = if x <= y then x else y

elemMin:: [Int] -> Int
elemMin[] = 0
elemMin[x] = x
elemMin(x:xs) = menor x (elemMin xs)

-- 12. Escribir una función que obtenga el enésimo número de la secuencia de Fibonacci.

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)

-- 13. Implementar una función que verifique si una cadena de texto es un palíndromo.

esPalindromo:: String -> Bool
esPalindromo [] = False
esPalindromo [x] = True
esPalindromo (x:xs) = x == last xs && esPalindromo (init xs)

-- 14. Definir una función que elimine los duplicados de una lista.

eliminarDuplicados :: [Int] -> [Int]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (x:xs) = if elem x xs then eliminarDuplicados xs
  else x : eliminarDuplicados xs
  
-- 15.a Implementar una función que obtenga el producto con un nro en particular de todos los elementos de una lista.

prodLista :: Int -> [Int] -> [Int]
prodLista _ [] = []
prodLista a (x:xs) = a * x : prodLista a xs

-- 15.b Implementar una función que obtenga el producto de todos los elementos de una lista.
prodLista2 :: [Int] -> [Int]
prodLista2 [] = []
prodLista2 (x:xs) = x * x : prodLista2 xs

main :: IO ()
main = do
  print (prodLista2 [2,2,2,2,2])
  