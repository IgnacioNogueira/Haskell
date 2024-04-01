divisionEntera :: Int -> Int -> Int
divisionEntera a b
  | b == 0 = error "No se puede dividir por cero"
  | a < b = 0
  | otherwise = 1 + divisionEntera (a - b) b

{-El guardia otherwise es una forma abreviada de decir "en cualquier otro caso"-}

{-En cada llamada recursiva, se realiza la resta (a - b) para obtener el nuevo dividendo, y se utiliza la misma variable b como divisor. Esto se debe a que en el método de las restas sucesivas, el divisor se mantiene constante durante el proceso de división.

La expresión divisionEntera (a - b) b se encarga de realizar una nueva iteración de la división entera utilizando el nuevo dividendo (a - b) y el mismo divisor b. Esto se repite hasta que se cumpla alguna de las condiciones de los guardias y se retorne un resultado.-}

main :: IO ()
main = do
  let resultado = divisionEntera 55 3
  print resultado
