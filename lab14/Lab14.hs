import Test.QuickCheck
import Control.Monad.Writer
import Control.Monad.List
import Control.Monad.State
----------- Side effects - debugging pure functions
f :: Float -> Float
f x = x + 1

g :: Float -> Float
g x = x * 2

f' :: Float -> (Float,String)
f' x = (x + 1,"f was called.")

g' :: Float -> (Float,String)
g' x = (x * 2, "g was called.")

bind1 :: (Float -> (Float,String)) -> ((Float,String) -> (Float,String))
bind1 f = (\(xg,sg) -> (fst (f xg), sg ++ snd (f xg)))

--((bind f') . g') 4

unit1 :: Float -> (Float,String)
unit1 x = (x,"")

lift1 :: (Float -> Float) -> (Float -> (Float,String))
lift1 f = unit1 . f

testEquality1 :: Float -> Bool
testEquality1 x = ((bind1 (lift1 f)).(lift1 g)) x == (lift1 (f.g)) x


----------- A Container : Multivalued functions

data Complex a = a :+ a
     deriving (Show, Eq)

f2 :: Complex Double -> Complex Double
f2 (x :+ y) = (x+1) :+ (y+1)

g2 :: Complex Double -> Complex Double
g2 (x :+ y) = (x * 2) :+ (y * 2)

bind2 :: (Complex Double -> [Complex Double]) -> ([Complex Double] -> [Complex Double])
bind2 f = (\listG -> concat [f lg| lg <- listG])

unit2 :: Complex Double -> [Complex Double]
unit2 c = [c]

lift2 :: (Complex Double -> Complex Double) -> (Complex Double -> [Complex Double])
lift2 f = unit2 . f

testEquality2 :: Double -> Bool
testEquality2 x = ((bind2 (lift2 f2)).(lift2 g2)) (x :+ x) == (lift2 (f2.g2)) (x :+ x)