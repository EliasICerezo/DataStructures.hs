-------------------------------------------------------------------------------
-- Representation for boards in 8-Puzzle
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module EightBoard
  ( Board
  , Position
  , readBoard
  , manhattan
  , oneMove
  , goal
  ) where

import Char(chr,ord)
import Array

type Position = (Int,Int)
data Board = Board (Array Int Position) deriving Eq

toChar :: Int -> Char
toChar 0            = ' '
toChar n 
 | n >= 1 && n <= 8 = chr (ord '0' + n)
 | otherwise        = error $ show n ++ " is not a digit in 0..8"

toDigit :: Char -> Int
toDigit c
 | c `elem` " 0"        = 0
 | c >= '1' && c <= '8' = ord c - ord '0'
 | otherwise            = error $ c : " is not a digit in 0..8 or a space"

get :: Position -> Board -> Int
get (i,j) (Board b) = head [ n | n <- [0..8], b!n == (i,j) ]

showBoard :: Board -> String
showBoard b = unlines [ [ toChar (get (i,j) b) | j <- [1..3] ] 
                      | i <- [1..3]  
                      ]

instance Show Board where
  show = showBoard

readBoard :: String -> Board
readBoard xs = Board (array (0,8) (zipWith f xs ps))
 where 
   ps = [ (i,j) | i <- [1..3], j <- [1..3] ]
   f c p = (toDigit c, p)

-- Goal configuration
goal :: Board
goal = readBoard "12345678 "


dist :: Position -> Position -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- Manhattan distance from b to goal
manhattan :: Board -> Int
manhattan b = sum $ zipWith dist (positions b) (positions goal)
 where
  positions (Board b) = [ b!i | i <- [0..8] ]

-- Boards obtained after doing a single move 
oneMove :: Board -> [Board]
oneMove (Board b) = map Board [ b//[(0,b!i),(i,b!0)] 
                            | i<-[1..8], dist (b!0) (b!i) == 1
                            ]


