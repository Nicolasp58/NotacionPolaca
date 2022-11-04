import Data.List

cond :: Float-> Float
cond x  |x == 3 = 100
        |x == 5 = 25
        |otherwise = 0

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:xs) "neg1" = -x:xs
            foldingFunction (x:xs) "raiz2" = sqrt x:xs
            foldingFunction (x:y:ys) "condnumero" = cond(x):y:ys
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs "product" = [product xs]
            foldingFunction xs "promedio" = (sum xs/fromIntegral (length xs)) :xs
            foldingFunction xs numberString = read numberString:xs


