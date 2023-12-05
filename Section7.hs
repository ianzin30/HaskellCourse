-- Section 7

-- último de forma recursiva
myLast :: [a] -> a
myLast [] = error "Lista vazia"
myLast [x] = x
myLast (_:xs) = myLast xs

-- "gira a lista" e pega o primeiro elemento
myLast2 :: [a] -> a
myLast2 y = head (reverse y)

-- antepenultimo

mySecondLast :: [a] -> a
mySecondLast [] = error "lista vazia"
mySecondLast [x] = error "só tem um"
--mySecondLast ()


duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs)

average :: [Int] -> Float
average [] = error "Lista vazia"
average x = somalista x / fromIntegral (lengthlista x) :: Float
  where
    somalista :: [Int] -> Float
    somalista [] = 0
    somalista (y:ys) = (fromIntegral y :: Float) + somalista ys
    lengthlista :: [Int] -> Int
    lengthlista [] = 0
    lengthlista (_:ys) = 1 + lengthlista ys


insertInto :: a -> [a] -> Int -> [a]
insertInto x xs 1 = x:xs
insertInto x (y:ys) n = y:insertInto x ys (n-1)

printLuiz :: String
printLuiz = "Luiz"

-- testar map
por2 :: Int -> Int
por2 x = x * 2