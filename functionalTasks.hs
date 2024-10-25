--Task 1a 
second1 :: [a] -> a
second1 x = head(tail x)

--Task 1b 
second2 :: [a] -> a
second2 x = x !! 1

--Task 1c
second3 :: [a] -> a
second3 (_:x:_) = x

--Task 2a (*)
xor1 :: Bool -> Bool -> Bool
xor1 x y | x == y = False
         | x /= y = True

--Task 2b
xor2 :: Bool -> Bool -> Bool
xor2 x y = if x == True && y == True
                then False
           else if x == False && y == False 
                then False 
           else True
           
           
--Task 2c
xor3 :: Bool -> Bool -> Bool 
xor3 x y = if x /= y then True else False

--Task 3
sumsqr :: Int -> [Int]
sumsqr n = [x^2 | x <- [1..n]]

--Task 4 
grid :: Int -> [(Int, Int)]
grid2 :: Int -> [(Int, Int)]
grid2 n = [(x,y) | x <- [0..n], y <- [0..n]]
grid n = [(x,y) | (x,y) <- grid2 n, x/=y]

--Task 5
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | y < x = euclid (x-y) y
           
--Task 6 

fastrev :: [a] -> [a]
fastrev xs = rev xs []

rev :: [a] -> [a] -> [a]
rev []     ys = ys
rev (x:xs) ys = rev xs (x:xs)


