-- Informatics 1 - Functional Programming 
-- Tutorial 9
--

import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (Go x) = [Go x]
split (Turn x) = [Turn x]
split (a :#: b) = (split a) ++ (split b) 

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join (x:[]) = x
join (x:xs) = x :#: (join xs)

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent a b = split a == split b

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = equivalent c (join (split c))

verify :: Command -> Bool
verify Sit = False
verify (a :#: b) = False
verify (Turn x) = True
verify (Go x) = True

prop_split :: Command -> Bool
prop_split xs = and([ verify x | x <- split(xs)])


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n x = join (replicate n x)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon x = copy 5 ((Go x) :#: (Turn 72))

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon x nr = copy nr ((Go x) :#: (Turn (360/5)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side 0 step angle = Sit
spiral side n step angle = (Go side) :#: (Turn angle) :#: (spiral (side+step) (n-1) step angle)



-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = join.j.split where
							  j [] = []
							  j (Sit:c) = j c
							  j (Go x : Go y : c) = j(Go (x+y) : (j c))
							  j (Go 0 : c) = (j c)
							  j (Turn x : Turn y : c) = j(Turn (x+y) : (j c))
							  j (Turn 0 : c) = (j c)
							  j (x:c) = x : (j c)



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x where
	f 0 = GrabPen red :#: Go 10
	f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
	g 0 = GrabPen blue :#: Go 10
	g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f(x-1)
	n = Turn 60
	p = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f (x-1) :#: m :#: m :#: f (x-1) :#: m :#: m :#: f (x-1) :#: m :#: m where
	f 0 = Go 5
	f x = f (x-1) :#: p :#: f (x-1) :#: m :#: m :#: f (x-1) :#: p :#: f (x-1)
	m = Turn (-60)
	p = Turn 60

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x where
    f = Go 5
    l 0 = GrabPen red
    l x = p :#: r(x-1) :#: f :#: m :#: l(x-1) :#: f :#: l(x-1) :#: m :#: f :#: r(x-1) :#: p
    r 0 = GrabPen blue
    r x = m :#: l(x-1) :#: f :#: p :#: r(x-1) :#: f :#: r(x-1) :#: p :#: f :#: l(x-1) :#: m
    p = Turn 90
    m = Turn (-90)

main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
