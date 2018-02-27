-- Lab 13

import Test.QuickCheck


-- Exercise 1

--fct1, fct2 :: ...
fct1 = Just (+3) 
fct2 = (+) <$> (Just 3)
 
fctTest :: Maybe Integer -> Bool
fctTest n = (fct1 <*> n) == (fct2 <*> n)

gen1, gen2 :: Integer -> Maybe (Integer -> Integer)
gen1 x = Just (+x)
gen2 x = (+) <$> (Just x)
 
genTest :: Integer -> Maybe Integer -> Bool
genTest x n = ((gen1 x) <*> n) == ((gen2 x) <*> n)

-- Exercise 2

testIdentity :: Maybe Integer -> Bool
testIdentity  v = (pure id <*> v) == v
 
testComposition :: Integer -> Maybe Integer -> Bool
testComposition x w = ( pure (.) <*> pure (+x) <*> pure (*x) <*> w ) == (pure (+x) <*> ( pure (*x) <*> w))
 
testHomomorphism :: Integer -> Integer -> Bool
testHomomorphism x y = ( pure (+y) <*> Just x ) == (pure ( (+y) x) )
 
testInterchange :: Integer -> Integer -> Bool
testInterchange x y = (pure (+x) <*> Just y) == ( pure ($ y) <*> pure (+x) )
              