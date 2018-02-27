-- Declarative Programming
-- Lab 3
--

import           Data.Char
import           Data.List
import           Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
--halveEvens xs = [x `div` 2|x <- xs, x `mod` 2 == 0]
halveEvens xs = [x `div` 2|x <- xs, x `mod` 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
--half :: Int -> Int
--half x | (x `mod` 2) ==0 = (x `div` 2) | otherwise = 
halveEvensRec (x:l) = if ((x `mod` 2) == 0) then [(x `div` 2)] ++ halveEvensRec(l) else  halveEvensRec(l)
halveEvensRec [] = []

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvensRec xs == halveEvens xs

--quickCheck prop_halveEvens


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, hi >= x]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi (x:xs) | x >= lo && hi >= x = [x] ++ (inRangeRec lo hi xs) | otherwise = (inRangeRec lo hi xs)
inRangeRec lo hi [] = []

-- -- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRange lo hi xs) == (inRangeRec lo hi xs)

-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version


countPositives :: [Int] -> Int
countPositives xs = length[x| x <- xs, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec (x:xs) = if(x >0) then 1 + countPositives(xs) else countPositives xs
countPositivesRec [] = 0;

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives(xs) == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round(fromIntegral x * 90/100)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [discount x | x<-xs, discount x <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec (x:xs) | discount x <= 19900 = discount x + pennypincherRec xs | otherwise = pennypincherRec xs
pennypincherRec [] = 0

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher x = pennypincher x == pennypincherRec x

--quickCheck prop_pennypincher

-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product[digitToInt x | x<-xs, isDigit(x)]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec (x:xs) | isDigit(x) = (digitToInt x) * multDigitsRec(xs) | otherwise = multDigitsRec xs
multDigitsRec [] = 1

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits x = multDigitsRec x == multDigits x


-- 6. capitalise
lower :: String -> String
lower xs = [toLower x|x <- xs]
-- List-comprehension version

capitalise :: String -> String
capitalise [] = []
capitalise x = [toUpper(head(x))] ++ lower(tail(x))

lowerRec :: String -> String
lowerRec [] = []
lowerRec (x:xs) = toLower x : lowerRec xs

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec (x:xs) = [toUpper(x)] ++ lowerRec(xs)
capitaliseRec [] = []

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise x = capitaliseRec x == capitalise x



-- 7. title

-- List-comprehension version

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x : [if ( length y >= 4 ) then capitalise y else lower y | y<-xs]

-- Recursive version
titleRec :: [String] -> [String]
titleRec []=[]
titleRec (x:xs) = capitaliseRec(x) : [ if ( length y >= 4 ) then capitaliseRec y else lowerRec y | y<-xs ]


-- mutual test
prop_title :: [String] -> Bool
prop_title x = titleRec x == title x

