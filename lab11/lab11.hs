data Dom a = Empty -- interval vid (multimea vida)
            | Full -- intervalul total (multimea totala)
            | Ran a a -- interval inchis [a,b]
            | (Dom a) :|: (Dom a) -- reuniunea a 2 intervale A U B 
            | (Dom a) :&: (Dom a) -- intersectia a 2 intervale A ∩ B
            deriving Show

--ex 1
instance (Eq d, Ord d) => Eq (Dom d) where 
    Empty == Empty = True
    Full == Full = True 
    (Ran a b) == (Ran c d) = (minimum[a, b] == maximum[c, d]) && (minimum[c, d] == maximum[a, b])
    _ == _ = False

--ex 2
exist :: (Ord a) => a -> Dom a -> Bool
exist a Empty = False 
exist a Full = True 
exist a (Ran b c)
    | (minimum[b, c] <= a) && (a <= maximum[b, c]) = True
    | otherwise = False 
exist a (d1 :|: d2) = (exist a d1) || (exist a d2)
exist a (d1 :&: d2) = (exist a d1) && (exist a d2)

--ex 3
overlap :: (Ord a) => Dom a -> Dom a -> Bool
overlap Empty a = False 
overlap a Empty = False 
overlap Full a = True 
overlap a Full = True
overlap (Ran a b) (Ran c d)
    | exist (maximum[a, b]) (Ran c d) = True 
    | exist (maximum[c, d]) (Ran a b) = True 
    | otherwise = False 
overlap _ _ = False

--ex 4
normalize :: Dom a -> Dom a
normalize ((a :&: b) :|: c) = ((a :|: c) :&: (b :|: c))
normalize ((a :|: b) :&: c) = ((a :&: c) :|: (b :&: c))
normalize (c :|: (a :&: b)) = ((c :|: a) :&: (c :|: b))
normalize (c :&: (a :|: b)) = ((c :&: a) :|: (c :&: b))

--ex 5
newtype SDom a = SDom(Dom a)

instance Monoid (SDom a) where 
    mempty = SDom(Empty)
    mappend (SDom a) (SDom b) = SDom (a :|: b)

newtype PDom a = PDom(Dom a)

instance Monoid (PDom a) where 
    mempty = PDom(Empty)
    mappend (PDom a) (PDom b) = PDom (a :&: b)

--ex 6
--prea naspa
optimize' :: (Ord a) => Dom a -> Dom a
optimize' (a :|: Empty) = a
optimize' (a :&: Empty) = Empty
optimize' (Empty :|: a) = a
optimize' (Empty :&: a) = Empty
optimize' (a :|: Full) = Full
optimize' (a :&: Full) = a
optimize' (Full :|: a) = Full
optimize' (Full :&: a) = a
optimize' (Ran a b) = (Ran a b)
optimize' ((Ran a b) :|: (Ran c d))
    | exist (maximum[a, b]) (Ran c d) = Ran (minimum[a, c]) (maximum[c, d])
    | exist (maximum[c, d]) (Ran a b) = Ran (minimum[a, c]) (maximum[a, b])
    | otherwise = ((Ran a b) :|: (Ran c d))
optimize' ((Ran a b) :&: (Ran c d))
    | ((maximum [a, b]) < (minimum[c, d])) || ((maximum[c, d]) < (minimum [a, b])) = Empty
    | otherwise = Ran (maximum[(minimum[a, b]), (minimum[c,d])])  (minimum[(maximum[a, b]), (maximum[c,d])])
optimize' (a :|: b) = (optimize' a) :|: (optimize' b)
optimize' (a :&: b) = (optimize' a) :&: (optimize' b)

optimize :: (Ord a) => Dom a -> Dom a
optimize d 
    | d == (optimize' d) = d 
    | otherwise = optimize (optimize' d)

--ex 7
data DomF a = EmptyF -- interval vid (multimea vida)
            | RanF a a -- interval inchis [a,b]
            | (DomF a) :||: (DomF a) -- reuniunea a 2 intervale A U B
            deriving Show

instance Foldable DomF where
    foldMap f EmptyF = mempty
    foldMap f (RanF a b) = f a `mappend` f b
    foldMap f (a :||: b) = foldMap f a `mappend` foldMap f b
    -- foldMap f (Ran a b :|: d) = f a mappend (foldMap f d) ??????

--ex 8
data Bin a = Leaf
           | Node a (Bin a) (Bin a)

instance Foldable Bin where
    foldMap f Leaf = mempty
    foldMap f (Node x l r) = foldMap f l `mappend`  
                             f x           `mappend`  
                             foldMap f r
 