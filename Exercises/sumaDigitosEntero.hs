-- Implementar una función que calcule la suma de los dígitos de un número entero.
-- Ejemplo: sumaDigitos 123 = 6
--          sumaDigitos 12345 = 15

digitos :: Int -> [Int]
digitos 0 = [] -- Condición de corte
digitos x = x `mod` 10 : digitos (x `div` 10)

suma:: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

main:: IO()

main = do
  let valores = digitos 1231231
  print(suma valores) 