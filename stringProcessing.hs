-- Haskell Coursework 1d
-- 
-- Lauryn Williams - Lewis 
-- psylw2@nottingham.ac.uk



import Data.Char(digitToInt)



-- Compression: Exercise 1 
-- selects a run of repeated characters from the start of a string with the run being as long as possible 

chomp :: String -> String
chomp x = takeWhile ( == head x ) x

-- Compresstion: Exercise 2
-- selects a run of repeated characters from the start of a string with the run comprimising of 9 characters
 
munch :: String -> String
munch = take 9.chomp

-- Comprehension: Exercise 3 
-- splits a string into a list of runs of repeated characters with each run comprimising of at  most 9 characters 

runs :: String -> [String] 
runs [] = []
runs xs = munch2 : runs (drop (length munch2) xs )
     where munch2 = munch xs 
     
-- drop removes elements
     
-- Comprehension : Exercise 4 
-- tranforms a string into a list of pairs comprimising of each letter from the run and the number of repetitions of that letter 

encode :: String -> [(Char, Int)]
encode xs = [(head x , length x)| x <- runs xs]

-- Comprehension: Exercise 5  
-- that flattens a list of pairs of characters and digits to a string 

flatten :: [(Char, Int)] -> String
flatten [] = []
flatten ((a,b): xs) = a : show b ++ flatten xs 
             
-- Comprehension: Exercise 6 
-- define a function using flatten and encode that compresses a string using run length encoding 

compress :: String -> String 
compress = flatten.encode


-- Decompresstion: Exercise 7 
-- define a function that performs the inverse to encode 

decode :: [(Char, Int)] -> String 
decode [] = []
decode ((a,b): xs) = replicate b a ++ decode xs

-- Decompression: Exercise 8 
-- define a function that performs the inverse function to flatten

expand :: String -> [(Char, Int)]
expand "" = []
expand (a:b:xs) =  (a, digitToInt b) : expand xs

-- Decompression: Exercise 9
-- using decode and expand that performs the inverse function to compress 

decompress :: String -> String
decompress = decode.expand


