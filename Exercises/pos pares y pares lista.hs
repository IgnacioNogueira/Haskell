-- Implementar una función que obtenga los elementos en posiciones pares de una lista.
-- Ejemplo: pares [1,2,3,4,5] = [1,3,5]
--          pares [5,4,3,2,1] = [5,3,1]

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:xs) = x : posPares xs

-- Implementar una función que obtenga los elementos pares de una lista.
-- Ejemplo: pares [1,2,3,4,5] = [1,3,5]
--          pares [5,4,3,2,1] = [5,3,1]
par::Int -> Int
par x = if x `mod` 2 == 0
then 1 else 0


pares::[Int] -> Int
pares [] = 0
pares(x:xs) = (par x) + pares xs

main:: IO()

main = do
  print(pares []) 