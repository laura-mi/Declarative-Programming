-- Informatics 1 - Functional Programming 
-- Declarative Programming 
-- Lab 5

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. Map
-- a. (4 simboluri)
uppers :: String -> String
--uppers xs = map toUpper xs
uppers = map toUpper
-- b. (7 simboluri)
doubleItem :: Int -> Int
doubleItem x = 2*x
doubles :: [Int] -> [Int]
--doubles xs = map doubleItem xs
doubles = map (* 2)

-- c. (10 simboluri)
penceToPounds :: [Int] -> [Float]
--penceToPounds = map ( \x -> (fromIntegral x)/ 100)
--penceToPounds = map $(/100).fromIntegral --10 simboluri
--penceToPounds = map ((/100).fromIntegral)
penceToPounds xs = map ((/100).fromIntegral) xs

-- d. (11 simboluri)
uppers2 :: String -> String
uppers2 xs = [toUpper x|x<-xs]

-- (8 simboluri)
prop_uppers :: String -> Bool
prop_uppers x= uppers2 x == uppers x



-- 2. Filter
-- a. (4 simboluri)
alphas :: String -> String
alphas = filter isAlpha

-- b. (8 simboluri)
rmChar ::  Char -> String -> String
rmChar x = filter (/= x)

-- c. (8 simboluri)
above :: Int -> [Int] -> [Int]
above nr = filter (> nr)

-- d. (13 simboluri)
unequals :: [(Int,Int)] -> [(Int,Int)]
--unequals = filter ( \(x,y) -> x /= y )
unequals = filter (\x -> fst x /= snd x)

-- e. (15 simboluri)
rmCharComp :: Char -> String -> String
rmCharComp y xs = [x | x<-xs, x /= y]

-- (11 simboluri)
prop_rmChar :: Char -> String -> Bool
prop_rmChar x xs = rmChar x xs == rmCharComp x xs



-- 3. Comprehensions vs. map & filter
-- a. 
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- (7 simboluri)
upperChars' :: String -> String
upperChars' = map toUpper.filter isAlpha

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b. 
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- (13 simboluri)
largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (*2).filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- (11 simboluri)
reverseEven' :: [String] -> [String]
reverseEven' = map reverse.filter (even.length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

-- (7 simboluri)
productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.  (16 simboluri)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec(xs)

-- (7 simboluri)
andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.  (17 simboluri)
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec(xs)

-- (8 simboluri)
concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.  (17 simboluri)
rmCharsRec :: String -> String -> String
rmCharsRec [] xs = xs
rmCharsRec (y:ys) xs = rmChar y (rmCharsRec ys xs)

-- (6 simboluri)
rmCharsFold :: String -> String -> String
rmCharsFold = flip $ foldr rmChar

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Int]]

-- 5
-- a. (10 simboluri)
uniform :: [Int] -> Bool
uniform xs = all (== head xs) xs

-- b. (	 simboluri)
valid :: Matrix -> Bool
valid xs = uniform (map length xs) && length xs > 0 && length (head xs) > 0 

-- 6.

-- 7.  (22 simboluri + 19 simboluri)  cu tot cu tratarea erorilor
plusRow :: [Int] -> [Int] -> [Int]
plusRow ys xs = if(length ys /= length xs) then error "Different nr of columns"
else zipWith (+) xs ys

plusM :: Matrix -> Matrix -> Matrix
plusM  m1 m2 = if(length m1 /= length m2) then error "Different nr of lines" else zipWith plusRow m1 m2

-- 8. (23 simboluri + 15 simboluri)  cu tot cu tratarea erorilor  
dot :: [Int] -> [Int] -> Int
dot x y = sum(zipWith (*) x y)

timesM :: Matrix -> Matrix -> Matrix
--timesM x y = zipWith dot x t where t = transpose(y)

