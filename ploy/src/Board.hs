module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars

import Data.List.Split

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Implementation Points              ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs


countcomma :: String -> Int
countcomma [] = 0
countcomma (x:xs) = if x == ',' then 1 + countcomma xs else countcomma xs

--check if there are 4 characters in the string
--lengths :: [String] -> [Int]
--lengths [] = []
--lengths (x:xs) = if length x > 4 then lengths xs else  length x : lengths xs 

--checkletter :: [String] -> Bool
--checkletter (x:xs) = if filter ((take 1) /= ("b" || "w")) x then checkletter xs else x : checkletter xs
stringtoint :: String -> Int
stringtoint s = read s :: Int

elembwtilln :: String -> Bool
elembwtilln [] = False
elembwtilln s = if length s >3 || elem ' '  s then False else if stringtoint s > 255 || stringtoint s < 0  then False else True


elembwtill255 :: String -> Bool
elembwtill255 [] = True
elembwtill255 (x:xs) = if x == 'b' || x == 'w'  then elembwtilln xs else False

elmmm :: [String] -> Bool
elmmm [] = True
elmmm (x:xs) = if elembwtill255 x || x == "" then elmmm xs else False


removenot :: [String] -> [String]
removenot [] = []
removenot (x:xs) = if x == "" || countcomma x /= 8 then removenot xs else x : removenot xs



validateFEN :: String -> Bool
validateFEN s = lengthList  (splitOn "/"  s)== 9  && lengthList (removenot (splitOn "/" s)) == 9 && elmmm (concat (map (splitOn ",") (splitOn "/"  s) ))   
--if lengthList (map elembwtill255 (concat (map (splitOn ",") (splitOn "/"  s) ))) == 81 then True else False

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 3 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

getCell :: String -> Cell
getCell [] = Empty
getCell (x:xs) = if x == 'b' then Piece Black (stringtoint xs) else if x == 'w' then Piece White (stringtoint xs) else Empty

getRow :: [String] -> [Cell]
getRow [] = []
getRow (x:xs) = getCell x : getRow xs

buildBoard :: String -> Board
buildBoard s = map getRow (map (splitOn ",") (splitOn "/"  s) ) 


-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Implementation Points    ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

horizontal :: Pos -> Pos -> [Pos]
horizontal x y
    | col x == col y = [x]
    | col x < col y = [x] ++ horizontal (Pos (succ (col x)) (row x)) y
    | otherwise = [x] ++ horizontal (Pos (pred (col x)) (row x)) y

vertical :: Pos -> Pos -> [Pos]
vertical x y
    | row x == row y = [x]
    | row x < row y = [x] ++ vertical (Pos (col x) (succ (row x))) y
    | otherwise = [x] ++ vertical (Pos (col x) (pred (row x))) y

diagonal :: Pos -> Pos -> [Pos]
diagonal x y
    |col x == col y && row x == row y = [x]
    |col x < col y && row x < row y = [x] ++ diagonal (Pos (succ (col x)) (succ (row x))) y
    |col x < col y && row x > row y = [x] ++ diagonal (Pos (succ (col x)) (pred (row x))) y
    |col x > col y && row x < row y = [x] ++ diagonal (Pos (pred (col x)) (succ (row x))) y
    |col x > col y && row x > row y = [x] ++ diagonal (Pos (pred (col x)) (pred (row x))) y


line :: Pos -> Pos -> [Pos]
line  x y 
    | col x == col y = vertical x y
    | row x == row y = horizontal x y
    | otherwise = diagonal x y
    
      
