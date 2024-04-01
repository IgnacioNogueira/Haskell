{-Compilado 01: Consignas Alfa
a) Dado dos números enteros A y B, implemente una función que retorne la división entera de ambos por el método de las restas sucesivas
-}

divisionEntera :: Int -> Int -> Either String Int
divisionEntera x y = if y == 0 then Left "Error, no se puede dividir por 0"
                     else Right (div x y)

main = do

  let resultado = divisionEntera 4 2

  case resultado of
    Right res -> putStrLn ("El resultado es: " ++ show res)
    Left err -> putStrLn err