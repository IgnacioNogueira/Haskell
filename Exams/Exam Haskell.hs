-- Dado dos números enteros A y B, implemente una función que retorne la división entera de ambos por el método de las restas sucesivas
-- Ejemplo: divEntera 10 3 = 3
--          divEntera 10 2 = 5

divRestasSucesivas :: Int -> Int -> Int
divRestasSucesivas x y = if (x < y) then 0 else 1 + divRestasSucesivas (x - y) y

{-
main :: IO ()
main = do
   print(divRestasSucesivas 55 44)
-}

-- Escribir una función para hallar la potencia de un número
-- Ejemplo: potencia 2 3 = 8
--          potencia 3 2 = 9

potenciaNro :: Int -> Int -> Int
potenciaNro x 0 = 1
potenciaNro x y = x * potenciaNro x (y - 1)

{-
main :: IO ()
main = do print (potenciaNro 2 4)
-}

-- Definir una función menor que devuelve el menor de sus dos argumentos enteros
-- Ejemplo: menor 3 4 = 3
--          menor 5 2 = 2

menorArgumentos :: Int -> Int -> Int
menorArgumentos x y = if x <= y then x else y

{-
main :: IO ()
main = do print (menorArgumentos 55 2)
-}

-- Definir una función maximoDeTres que devuelve el máximo de sus argumentos enteros
-- Ejemplo: maximoDeTres 6 2 3 = 6
--          maximoDeTres 5 8 1 = 8
--          maximoDeTres 3 4 5 = 5
maximo :: Int -> Int -> Int
maximo x y = if x >= y then x else y

maximoDeTres :: Int -> Int -> Int -> Int
maximoDeTres x y z = maximo x (maximo y z)

{-
main :: IO ()
main = do print(maximoDeTres 3 4 5)
-}

-- Escribir una función que sume dos números enteros.
-- Ejemplo: suma 3 4 = 7
--          suma 5 2 = 7
suma :: Int -> Int -> Int
suma x y = x + y

{-
main :: IO ()
main = do print(suma 5 2)
-}

-- Implementar una función que calcule el área de un círculo dado su radio.
-- Ejemplo: areaCirculo 3 = 28.274333882308138
--          areaCirculo 5 = 78.53981633974483
areaCirculo :: Float -> Float
areaCirculo r = pi * r * r

{-
main :: IO ()
main = do print(areaCirculo 3)
-}

-- Definir una función que determine si un número es par o impar
-- Ejemplo: esPar 4 = True
--          esPar 5 = False

esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

{-
main :: IO ()
main = do print(esPar 6)
-}

-- Implemente una función recursiva para calcular el factorial de un número
-- Ejemplo: factorial 5 = 120
--          factorial 3 = 4
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

{-
main :: IO ()
main = do print(factorial 5)
-}

-- Implemente una función recursiva para calcular la serie gauss de un número
-- Ejemplo: serieGauss 100 = 5050

serieGauss :: Int -> Int
serieGauss 0 = 0
serieGauss x = x + serieGauss (x - 1)

{-
main :: IO ()
main = do print(serieGauss 100)
-}

-- Implemente una función recursiva para calcular el fibonacci de un número
-- Ejemplo: fibonacci 5 = 5
--          fibonacci 3 = 2
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 2) + fibonacci (x - 1)

{-
main :: IO ()
main = do print(fibonacci 3)
-}

-- Implementar una función que invierta una lista
-- Ejemplo: invertir [1,2,3] = [3,2,1]
--          invertir [5,4,3,2,1] = [1,2,3,4,5]

invertir :: [a] -> [a]
invertir [] = []
invertir (x : xs) = invertir xs ++ [x]

{-
main :: IO ()
main = do print(invertir [1,2,3])
-}

-- Definir una función que determine si una lista está ordenada de forma ascendente
-- Ejemplo: ordenada [1,2,3] = True
--          ordenada [5,4,3,2,1] = False

ordenada :: [Int] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x : y : xs) = x <= y && ordenada (y : xs)

{-
main :: IO ()
main = do print(ordenada [1])
-}

-- Escribir una función que cuente la cantidad de elementos en una lista.
-- Ejemplo: cantidad [1,2,3] = 3
--          cantidad [5,4,3,2,1] = 5

cantidad :: [a] -> Int
cantidad [] = 0
cantidad (x : xs) = 1 + cantidad xs

{-
main :: IO ()
main = do print(cantidad [1,2,3])
-}

-- Implementar una función que obtenga los elementos en posiciones pares de una lista.
-- Ejemplo: pares [1,2,3,4,5] = [1,3,5]
--          pares [5,4,3,2,1] = [5,3,1]

pares :: [a] -> [a]
pares [] = []
pares [x] = [x]
pares (x : y: xs) = x : pares xs

{-main :: IO ()
main = do print(pares [1,2])--}

-- Definir una función que calcule el máximo común divisor de dos números.
-- Ejemplo: mcd 10 5 = 5
--          mcd 5 3 = 1

mcd :: Int -> Int -> Int
mcd x 0 = x
mcd x y = mcd x (mod x y)

{-
main :: IO ()
main = do print(mcd 10 5)
-}

-- Implementar una función que calcule la suma de los dígitos de un número entero.
-- Ejemplo: sumaDigitos 123 = 6
--          sumaDigitos 12345 = 15

sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos x = x `mod` 10 + sumaDigitos (x `div` 10)

{-
main :: IO ()
main = do print(sumaDigitos 102)
-}

-- Definir una función que encuentre el elemento mínimo en una lista.
-- Ejemplo: minimo [1,2,3] = 1
--          minimo [5,4,3,2,1] = 1

menor :: Int -> Int -> Int
menor x y = if x <= y then x else y

minimo :: [Int] -> Int
minimo [] = 0
minimo [x] = x -- si viene un solo valor, devuelvo el mismo
minimo (x : xs) = menor x (minimo xs)
 
{-
main :: IO ()
main = do print(minimo [5,4,3,2,1])
-}

-- PARCIAL: Implementar una función que obtenga
-- los elementos pares de una lista.

par :: Int -> [Int]
par x = if x `mod` 2 == 0 then [x] else []

paresLista :: [Int] -> [Int]
paresLista [] = []
paresLista (x : xs) = par x ++ paresLista xs

{-
main :: IO ()
main = do print (paresLista [5, 4, 3, 2, 1])
-}

esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo [x] = True
esPalindromo (x:xs) = x == last xs && esPalindromo(init xs)

-- Definir una función que elimine los duplicados de una lista.
-- Ejemplo: eliminarDuplicados [1,2,3,1,2] = [1,2,3]
--          eliminarDuplicados [5,4,3,2,1] = [5,4,3,2,1]

eliminarDuplicados :: [Int] -> [Int]
eliminarDuplicados [] = []
eliminarDuplicados [x] = [x]
eliminarDuplicados (x:xs) = if elem x xs
  then eliminarDuplicados xs
  else x : eliminarDuplicados xs

ponerlosEnOrdenAsc:: [Int] -> [Int]
ponerlosEnOrdenAsc [] = []
ponerlosEnOrdenAsc [x] = [x]
ponerlosEnOrdenAsc (x:y:xs) = if x > y then y: ponerlosEnOrdenAsc(x:xs) else x: ponerlosEnOrdenAsc(y:xs)

eliminarDuplicadosYOrdenar :: [Int] -> [Int]
eliminarDuplicadosYOrdenar x = ponerlosEnOrdenAsc (eliminarDuplicados x)

{-
main :: IO()
main = do print (eliminarDuplicadosYOrdenar [1,2,3,1,2])
-}

-- Implementar una función que obtenga el producto de todos los elementos de una lista.
-- Ejemplo: producto [1,2,3] = 6
--          producto [5,4,3,2,1] = 120

producto::[Int] -> Int
producto [] = 1
producto x = foldl(*) 1 x 

producto2::[Int] -> Int
producto2 [] = 1
producto2 (x:xs) = x * producto2 xs

{-
main :: IO()
main = do print (producto2 [5,4,3,2,1])
-}

{-Ejercicios:
1. Hacer funciones para manejo de Cola (`enqueue` / `dequeue`)
2. Hacer funciones para manejo de Pila (`push` / `pop`)
-}

--1. Hacer funciones para manejo de Cola (`enqueue` / `dequeue`)

enqueue :: a -> [a] -> [a]
enqueue x xs = xs ++ [x]  

dequeue :: [a] -> (a, [a])
dequeue [] = error "cola vacia"
dequeue (x:xs) = (x, xs)

-- 2. Hacer funciones para manejo de Pila (`push` / `pop`)

push :: a -> [a] -> [a]
push x xs = x:xs 

pop :: [a] -> (a, [a])
pop (x:xs) = (x, xs)
pop [] = error "pila vacia"





