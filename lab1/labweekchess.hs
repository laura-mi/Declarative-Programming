-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck
import Data.Char



-- Exercise 8:

pic1 :: Picture
pic1 = beside knight (invert knight)

pic2 :: Picture
pic2 = above pic1 (invert pic1)

pic3 :: Picture
pic3 = above pic1 (flipV pic1)


-- Exercise 9:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

whiteRow :: Picture
whiteRow = over ((beside (beside (beside rook knight) (beside bishop queen)) (beside (beside king bishop) (beside knight rook)))) otherEmptyRow

blackRow :: Picture
blackRow = over (beside (beside (beside (beside (beside (beside (beside (invert rook) (invert knight)) (invert bishop)) (invert queen)) (invert king)) (invert bishop)) (invert knight)) (invert rook)) emptyRow

-- e)
blackPawns :: Picture
blackPawns = over (repeatH 8 (invert pawn)) otherEmptyRow

whitePawns :: Picture
whitePawns = over (repeatH 8 (pawn)) emptyRow

populatedBoard :: Picture
populatedBoard = above (above (above (above blackRow blackPawns) middleBoard) whitePawns) whiteRow



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 10:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = beside (twoAbove x) (invert (twoAbove x))

-- Extra
emptyBoard::Picture
emptyBoard=above (above (above (above emptyRow otherEmptyRow) middleBoard) emptyRow) otherEmptyRow