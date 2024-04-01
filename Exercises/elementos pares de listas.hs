{--- Implementar una función que obtenga los elementos pares de una lista.
-- Ejemplo: pares [1,2,3,4,5] = [1,3,5]
--          pares [5,4,3,2,1] = [5,3,1]-}

pares :: [Int] -> [Int]
pares lista = [x | x <- lista, even x]

main :: IO ()
main = do
  let lista = [1,2,3,4,5]
  print (pares lista)  -- Resultado: [2,4]
