misterio :: [Int] -> Int
misterio [] = 0 {-- esta es la condición de corte--}
misterio (x:xs) = 1 + misterio xs {-head siempre es el primer valor
se suma la cantidad de veces que entra y se suma
-}
main = print (misterio [1, 2, 3, 4, 5,9])