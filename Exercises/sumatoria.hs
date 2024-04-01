module Main where

sumatoria:: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)


main = print (sumatoria 10)