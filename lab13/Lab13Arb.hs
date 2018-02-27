module Lab13Arb where
import Control.Applicative --hiding (fmap, (<*>), pure)
--import Prelude hiding (fmap, (<*>), pure)
import Test.QuickCheck





--Exercise 3
data Arb a = Nil | Node a (Arb a) (Arb a) 
             deriving (Show,Eq)
              
--let exTree = Node 4 (Node 25 Nil Nil) (Node 16 Nil Nil) :: Arb Float

instance Functor Arb where
    fmap f Nil = Nil
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)  

instance Applicative Arb where
    pure x = Node x (pure x) (pure x)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Node f leftf rightf) <*> (Node a left right) =  Node (f a) (leftf <*> left) (rightf <*> right)  
 
-- (pure sqrt) <*> exTree

listToArb :: [a] -> Arb a
listToArb (x:xs) = Node x (listToArb xs) Nil
 

testIdentity :: Maybe Integer -> Bool
testIdentity  v = (pure id <*> v) == v
 
 
testComposition :: Integer -> Maybe Integer -> Bool
testComposition x w = ( pure (.) <*> pure (+x) <*> pure (*x) <*> w ) == (pure (+x) <*> ( pure (*x) <*> w))
 
testHomomorphism :: Integer -> Integer -> Bool
testHomomorphism x y = ( pure (+y) <*> Just x ) == (pure ( (+y) x) )
 
testInterchange :: Integer -> Integer -> Bool
testInterchange x y = (pure (+x) <*> Just y) == ( pure ($ y) <*> pure (+x) )










              
