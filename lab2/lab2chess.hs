-- Informatics 1 - Functional Programming
-- Lab week tutorial part II
--
--

import           Data.Char
import           PicturesSVG
import           Test.QuickCheck
import Data.List.Split

-- Exercise 1
-- write the correct type and the definition for
isFENChar :: Char -> Bool
isFENChar x = (elem (toLower x) ['r','n','b','q','k','p']) || isDigit(x) || (x == '/')

-- Exercise 2.a
-- write a recursive definition for
besideList :: [Picture] -> Picture
besideList (x:l) = beside x (besideList l)
besideList [] = Empty


-- Exercise 2.c
-- write the correct type and the definition for
toClear :: Int -> Picture
toClear n = besideList (replicate n clear)

-- Exercise 3
-- write the correct type and the definition for
fenCharToPicture :: Char -> Picture
fenCharToPicture c | c == 'r' = invert rook
 | c == 'R' = rook
 | c == 'n' = invert knight
 | c == 'N' = knight
 | c == 'b' = invert bishop
 | c == 'B' = bishop
 | c == 'k' = invert king
 | c == 'K' = king
 | c == 'p' = invert pawn
 | c == 'P' = pawn
 | c == 'q' = invert queen
 | c == 'Q' = queen
 | c >= '1' && c <= '8' = toClear(digitToInt c)
 | otherwise = Empty

-- Exercise 4
-- write the correct type and the definition for
isFenRow :: [Char] -> Bool
isFenRow (x:l) = isFENChar(x) && isFenRow(l)
isFenRow [] = True

-- Exercise 6.a
-- write a recursive definition for
fenCharsToPictures :: String -> [Picture]
fenCharsToPictures (x:l) = [fenCharToPicture(x)] ++ fenCharsToPictures(l)
fenCharsToPictures [] = [Empty]

-- Exercise 6.b
-- write the correct type and the definition of
fenRowToPicture :: [Char] -> Picture
fenRowToPicture x = besideList(fenCharsToPictures(x))

-- Exercise 7
-- write the correct type and the definition of
-- mySplitOn 
-- splitOn "x" "abxabcxd"

-- Exercise 8.a
-- write a recursive definition for
fenRowsToPictures :: [String] -> [Picture]
fenRowsToPictures (x:l) = [fenRowToPicture(x)] ++ fenRowsToPictures(l)
fenRowsToPictures [] = [Empty]

-- Exercise 8.b
-- write the correct type and the definition of
aboveList :: [Picture] -> Picture
aboveList (x:l) = above x (aboveList l)
aboveList [] = Empty

-- Exercise 8.c
-- write the correct type and the definition of
fenToPicture :: [String] -> Picture
fenToPicture x = aboveList(fenRowsToPictures(x))

-- Exercise 9
-- write the correct type and the definition of
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)
-- b)
otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)
-- c)
middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)
emptyBoard::Picture
emptyBoard=above (above (above (above emptyRow otherEmptyRow) middleBoard) emptyRow) otherEmptyRow

chessBoard :: [String] -> Picture
chessBoard x = over (fenToPicture(x)) emptyBoard

-- Exercise 10
-- write the correct type and definition of
--allowedMoved :: Picture -> 
