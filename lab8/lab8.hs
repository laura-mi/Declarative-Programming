-- Informatics 1 Functional Programming
-- Tutorial 8
--

import System.Random


-- Importing the keymap module

--import KeymapList
import KeymapTree

-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum ([length p | (y, (p,u)) <- xs])

formatLine :: Int -> (Barcode, Item) -> String
formatLine lg (code,(p,u)) = code ++ "..." ++ p ++ (replicate (lg-(length p)) '.') ++ "..." ++ u

showCatalogue :: Catalogue -> String
showCatalogue xs = unlines([formatLine val x | x<-c]) where												
												c = toList xs
												val = (longestProductLen c)

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just (head xs)

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = maybeToList x ++ catMaybes xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems xs cat = catMaybes [get x cat|x<-xs]


---
--theDB <- readDB
--size theDB
--getSample theDB
--get it theDB --(‘it’ is a GHCi-special that refers to the value returned by the last expression it evaluated).
-- :set +s --GHCi will now give you time (and memory use) information for each expression you ask it to evaluate.



-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)