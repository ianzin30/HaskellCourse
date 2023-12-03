ian :: String
ian = "ian"


double :: Int -> Int
double x = x*2

perimeter :: Int -> Int -> Int
perimeter x y = 2*x + 2*y

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

valormodulo :: Int -> Int
valormodulo n
  | n >= 0 = n
  | otherwise = -n

fastExp :: Integer -> Integer -> Integer
fastExp _ 0 = 1
fastExp x n =
  let y = fastExp x n_halved
      n_halved = div n 2
  in 
    if even n
    then y * y
    else y * y * x 

power :: Int -> Int -> Int 
power x 0 = 1
power x n = x * power x (n-1)

powerfast :: Int -> Int -> Int
powerfast x 0 = 1
powerfast x n 
  | even n = y * y
  | otherwise = y * y * x 
  where
      y = powerfast x (n `div` 2)

isPrime :: Int -> Bool

isPrime 0 = False
isPrime 1 = False
isPrime x = not (hasDivisor (x-1))
    where
        hasDivisor :: Int -> Bool
        hasDivisor 1 = False
        hasDivisor n = (x `mod` n) == 0 || hasDivisor (n-1)
        