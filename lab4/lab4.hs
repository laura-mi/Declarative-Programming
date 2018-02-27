-- Declarative Programming
-- Lab 4
--


import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs = if(n < 0 || n> length xs) then error "n is too big" else (drop n xs ++ take n xs)

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey x = zip['A'..'Z'] (rotate x ['A'..'Z'])

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp x xs = if(length list == 0) then x else head(list) where list = [y|(xx,y)<-xs, xx == x]

-- 5.
encipher :: Int -> Char -> Char
encipher n x = lookUp x (makeKey n)

-- 6.
normalize :: String -> String
normalize xs = [ toUpper x | x<-xs, isLetter x || isDigit x]

-- 7.
encipherStr :: Int -> String -> String
encipherStr n xs = [encipher n y| y<-ys] where ys = (normalize xs)

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey l = [(y,x)|(x,y)<-l]

-- 9.
decipher :: Int -> Char -> Char
decipher n x = lookUp x (reverseKey(makeKey n))

decipherStr :: Int -> String -> String
decipherStr n xs = [decipher n y|y<-xs]

-- 10.
prop_cipher :: Int -> String -> Property
prop_cipher k xs = (k >= 0 && k <= length ['A'..'Z']) ==> decipherStr k (encipherStr k xs) == normalize xs

-- 11.
contains :: String -> String -> Bool
contains x [] = False
contains x xs = if(isPrefixOf x xs) then True else contains x (tail(xs))

-- 12.
candidates :: String -> [(Int, String)]
candidates xs = [(key, (decipherStr key xs)) |key<-[1..26], contains "AND" (decipherStr key xs) || contains "THE" (decipherStr key xs)]


-- Optional Material

-- 13.
splitEachFive :: String -> [String]
splitEachFive = undefined

-- 14.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 15.
encrypt :: Int -> String -> String
encrypt = undefined

-- 16.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 17.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 18
freqDecipher :: String -> [String]
freqDecipher = undefined

