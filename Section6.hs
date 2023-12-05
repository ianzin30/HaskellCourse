-- section 6, lists

soma :: [Int] -> Int 
soma [] = 0
soma (x:xs) = x + soma xs

firstandsecond :: [Int] -> (Int, Int)
firstandsecond a =
    let (x:y:xs) = a
    in  (x,y)

soma2 :: [Int] -> Int
soma2 lista =
    case lista of
        [] -> 0
        x:xs -> x + soma2 xs

-- length, null, tail, head, init, last, >, <, [char] == String
