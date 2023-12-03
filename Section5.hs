-- section 5

relogio :: Int -> (Int, Int, Int)
relogio seconds = (h,m,s)
    where
        h = seconds `div` 3600
        m = (seconds-(h*3600)) `div` 60
        s = seconds `mod` 60
    
    

first :: (Int,Int,Int) -> Int
first (x,_,_) = x

second :: (Int,Int,Int) -> Int
second (_,x,_) = x

third :: (Int,Int,Int) -> Int
third (_,_,x) = x

distance :: (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)


distance2 :: (Float,Float) -> (Float,Float) -> Float
distance2 p1 p2 = sqrt (dx**2 + dy**2)
    where
        (x1,y1) = p1
        (x2,y2) = p2
        dx  = x1 - x2 
        dy  = y1 - y2 