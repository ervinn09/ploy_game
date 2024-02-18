module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255


--sampleBoard :: Board
--sampleBoard = [[Empty,Piece White 85,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Empty,Empty,Piece White 1,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Empty,Piece White 2,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

--sampleBoard1 :: Board
--sampleBoard1 = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

--sampleBoard2 :: Board
--sampleBoard2 = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

typeOf :: Cell -> Cell
typeOf Empty = Empty
typeOf (Piece p i) = Piece p 1

checknow :: [Cell] -> Bool
checknow [] = True
checknow b = if  ( Piece White 170 `elem` b || Piece White 85 `elem` b) then False else True

checknowhite :: Board -> Bool
checknowhite [] = True
checknowhite b = checknow (concat(b)) 

checknob :: [Cell] -> Bool
checknob [] = True
checknob b = if  ( Piece Black 170 `elem` b || Piece Black 85 `elem` b) then False else True

checknoblack  :: Board -> Bool
checknoblack [] = True
checknoblack b = checknob (concat(b))
 
checknobwempty :: [Cell] -> Int
checknobwempty [] = 0
checknobwempty (x:xs) = if typeOf x == Piece White 1 then 1 + checknobwempty xs else checknobwempty xs

checknobwe :: Board -> Bool
checknobwe b = if checknobwempty (concat(b)) > 1  then False else True

checknobwempty2 :: [Cell] -> Int
checknobwempty2 [] = 0
checknobwempty2 (x:xs) = if typeOf x == Piece Black 1 then 1 + checknobwempty2 xs else checknobwempty2 xs

checknobwe2 :: Board -> Bool
checknobwe2 b = if checknobwempty2 (concat(b)) > 1  then False else True

gameFinished :: Board -> Bool
gameFinished b = if (checknowhite b == True || checknobwe b == True ) || (checknoblack b == True || checknobwe2 b == True) then True else False
 

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Implementation Points            ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

chartonum :: Char -> Int
chartonum 'a' = 0
chartonum 'b' = 1
chartonum 'c' = 2
chartonum 'd' = 3
chartonum 'e' = 4
chartonum 'f' = 5
chartonum 'g' = 6
chartonum 'h' = 7
chartonum 'i' = 8

--get the color of the piece
getcolor :: Cell -> String
getcolor (Piece White _) = "white"
getcolor (Piece Black _) = "black"

-- when the way is empty returns True
getCell1 :: Board -> Pos -> Cell
getCell1 b (Pos c r) = b !! (9-r) !! chartonum c

line1free :: [Pos] -> Board -> Cell
line1free [] _ = Empty
line1free (p:ps) b = if getCell1 b p == Empty then line1free ps b else getCell1 b p

wegisfree :: Board -> Move -> Bool
wegisfree b (Move (Pos sc sr) (Pos tc tr) r) = line1free (tail (line (Pos sc sr) (Pos tc tr))) b == Empty 


--wayissamecoloroccupied :: Board -> Move -> String
--wayissamecoloroccupied b (Move (Pos sc sr) (Pos tc tr) r) = if wegisfree b (Move (Pos sc sr) (Pos tc tr) r) == False then getcolor (line1free (tail (line (Pos sc sr) (Pos tc tr))) b) else "Empty" 

--wayisoccupied :: Board -> Move -> Bool
--wayisoccupied b (Move (Pos sc sr) (Pos tc tr) r) = if getcolor (getCell1 b (Pos sc sr)) == wayissamecoloroccupied b (Move (Pos sc sr) (Pos tc tr) r) then False else True
-- /= getcolor (getCell1 b (Pos sc sr)) 

postocells :: Board -> [Pos] -> [Cell]
postocells b [] = []
postocells b (p:ps) = getCell1 b p : postocells b ps

remnumofpiece :: Cell -> Cell
remnumofpiece Empty = Empty
remnumofpiece (Piece p i) = Piece p 1

pieceremin :: [Cell] -> [Cell]
pieceremin [] = []
pieceremin (x:xs) = foldr (:) [] (remnumofpiece x : pieceremin xs)

blackwayisoccupied :: Board -> Move -> Bool
blackwayisoccupied b (Move (Pos sc sr) (Pos tc tr) r) = if getcolor (getCell1 b (Pos sc sr)) == "black" && (Piece Black 1  `elem` (map remnumofpiece(postocells b (init(tail (line (Pos sc sr) (Pos tc tr)))))) || Piece White 1 `elem` (map remnumofpiece(postocells b (init(tail (line (Pos sc sr) (Pos tc tr)))))))  then True else False

whitewayisoccupied :: Board -> Move -> Bool
whitewayisoccupied b (Move (Pos sc sr) (Pos tc tr) r) = if getcolor (getCell1 b (Pos sc sr)) == "white" && (Piece Black 1  `elem` (map remnumofpiece(postocells b (init(tail (line (Pos sc sr) (Pos tc tr)))))) || Piece White 1 `elem` (map remnumofpiece(postocells b (init(tail (line (Pos sc sr) (Pos tc tr)))))))  then True else False

blackwayisoccupiedsamecolor :: Board -> Move -> Bool
blackwayisoccupiedsamecolor b (Move (Pos sc sr) (Pos tc tr) r) = if getcolor (getCell1 b (Pos sc sr)) == "black" &&  Piece Black 1 `elem` (map remnumofpiece (postocells b (tail (line (Pos sc sr) (Pos tc tr))))) then True else False

whitewayisoccupiedsamecolor :: Board -> Move -> Bool
whitewayisoccupiedsamecolor b (Move (Pos sc sr) (Pos tc tr) r) = if getcolor (getCell1 b (Pos sc sr)) == "white" &&  Piece White 1 `elem` (map remnumofpiece (postocells b (tail (line (Pos sc sr) (Pos tc tr))))) then True else False

--rotation
rot1 :: Board -> Move -> Bool
rot1 b (Move (Pos sc sr) (Pos tc tr) r) 
            |   r /= 0 && (Pos sc sr) /=  (Pos tc tr)  = isValidMove1 b (Move (Pos sc sr) (Pos tc tr) r)
            |   r /= 0 && (Pos sc sr) ==  (Pos tc tr) = True
            |   otherwise = False

isValidMove1 :: Board -> Move -> Bool
isValidMove1 b (Move (Pos sc sr) (Pos tc tr) r) 
    | getCell1 b (Pos sc sr) == Empty = False
    |  getcolor (getCell1 b (Pos sc sr)) == "white" && wegisfree b (Move (Pos sc sr) (Pos tc tr) r)  = True
    |  getcolor (getCell1 b (Pos sc sr)) == "black" && wegisfree b (Move (Pos sc sr) (Pos tc tr) r)  = True
    |  getcolor (getCell1 b (Pos sc sr)) == "white" && whitewayisoccupiedsamecolor b (Move (Pos sc sr) (Pos tc tr) r)  = False
    |  getcolor (getCell1 b (Pos sc sr)) == "black" && blackwayisoccupiedsamecolor b (Move (Pos sc sr) (Pos tc tr) r)  = False
    |  getcolor (getCell1 b (Pos sc sr)) == "white" && whitewayisoccupied b (Move (Pos sc sr) (Pos tc tr) r)  = False
    |  getcolor (getCell1 b (Pos sc sr)) == "black" && blackwayisoccupied b (Move (Pos sc sr) (Pos tc tr) r) = False
    | otherwise = True

isValidMove :: Board -> Move -> Bool
isValidMove b (Move (Pos sc sr) (Pos tc tr) r) 
    | r == 0 = isValidMove1 b (Move (Pos sc sr) (Pos tc tr) 0)
    | r /= 0 = rot1 b (Move (Pos sc sr) (Pos tc tr) r)



-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Implementation Points              ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################


--possibleMovesShield :: Pos -> Cell -> [Move]
--possibleMovesShield (Pos c r) (Piece p i) 
--                |  Piece (p (sumofb (inttobinary i))) == Piece p 1 then [Move (Pos c r) (Pos c r) 0]

inttobinary1 :: Int -> [Int]
inttobinary1 0 = []
inttobinary1 n = n `mod` 2 : inttobinary1 (n `div` 2)

inttobinary :: Int -> [Int]
inttobinary 0 = []
inttobinary x = reverse (inttobinary1 x)

--numofonesinalist
sumofb :: [Int] -> Int
sumofb [] = 0
sumofb (x:xs) = x + sumofb xs

twopot :: Int -> Int
twopot 0 = 1
twopot n = 2 * twopot (n-1)

--[0,1,0,1] = [4,1]
twopotenttoones :: [Int] -> [Int]
twopotenttoones [] = []
twopotenttoones (x:xs) = if x == 1 then twopot (length xs) : twopotenttoones xs else twopotenttoones xs


directionbin :: Cell -> Pos -> Pos
directionbin (Piece p i) x
                |  i == 1 =  (Pos (col x) (succ (row(x)))) 
                |  i == 2 =  (Pos (succ (col x)) ( succ (row(x))))
                |  i == 4 =  (Pos (succ (col x)) (row(x)))
                |  i == 8 =  (Pos (succ (col x)) (pred (row(x))))
                |  i == 16 =  (Pos (col x) (pred (row(x))))
                |  i == 32 =  (Pos (pred (col x)) (pred (row(x))))
                |  i == 64 = (Pos (pred (col x)) (row(x)))
                |  i == 128 = (Pos (pred (col x)) (succ (row(x))))

directionbin2 :: Cell -> Pos -> Pos
directionbin2 (Piece p i) x
                |  i == 1 =  (Pos (col x) (succ(succ (row(x)))))
                |  i == 2 =  (Pos (succ(succ (col x))) ( succ(succ (row(x)))))
                |  i == 4 =  (Pos (succ(succ (col x))) (row(x)))
                |  i == 8 =  (Pos (succ(succ (col x))) (pred(pred (row(x)))))
                |  i == 16 =  (Pos (col x) (pred(pred (row(x)))))
                |  i == 32 =  (Pos (pred(pred (col x))) (pred(pred (row(x)))))
                |  i == 64 = (Pos (pred(pred (col x))) (row(x)))
                |  i == 128 = (Pos (pred(pred (col x))) (succ(succ (row(x)))))

directionbin3 :: Cell -> Pos -> Pos
directionbin3 (Piece p i) x
                |  i == 1 =  (Pos (col x) (succ(succ(succ (row(x))))))
                |  i == 2 =  (Pos (succ(succ(succ (col x)))) ( succ(succ(succ (row(x))))))
                |  i == 4 =  (Pos (succ(succ(succ (col x)))) (row(x)))
                |  i == 8 =  (Pos (succ(succ(succ (col x)))) (pred(pred(pred (row(x))))))
                |  i == 16 =  (Pos (col x) (pred(pred(pred (row(x))))))
                |  i == 32 =  (Pos (pred(pred(pred (col x)))) (pred(pred(pred (row(x))))))
                |  i == 64 = (Pos (pred(pred(pred (col x)))) (row(x)))
                |  i == 128 = (Pos (pred(pred(pred (col x)))) (succ(succ(succ (row(x))))))
            
directionlist :: Cell -> Pos -> [Pos]
directionlist (Piece p i) x
                 |length (twopotenttoones(inttobinary i)) == 1 = [directionbin (Piece p i) x]
                  |length (twopotenttoones(inttobinary i)) == 2 = [directionbin2 (Piece p (head(twopotenttoones(inttobinary i)))) x, directionbin2 (Piece p (last(twopotenttoones(inttobinary i)))) x]
                  |length (twopotenttoones(inttobinary i)) == 3 = [directionbin3 (Piece p (head(twopotenttoones(inttobinary i)))) x, directionbin3 (Piece p (last(twopotenttoones(inttobinary i)))) x, directionbin3 (Piece p (head(tail(twopotenttoones(inttobinary i))))) x]
                  |length (twopotenttoones(inttobinary i)) == 4 = [directionbin (Piece p (head(twopotenttoones(inttobinary i)))) x, directionbin (Piece p (last(twopotenttoones(inttobinary i)))) x, directionbin (Piece p (head(tail(twopotenttoones(inttobinary i))))) x, directionbin (Piece p (last(init(twopotenttoones(inttobinary i))))) x]

-- letzte elemten der Liste rausschmeißen 
lineminus :: Pos -> Pos -> [Pos]
lineminus (Pos c r) (Pos c1 r1) = init(line (Pos c r) (Pos c1 r1)) 

--Board boundary
isvalidpos :: Pos -> Bool
isvalidpos (Pos c r) = if c >= 'a' && c <= 'i' && r >= 1 && r <= 9 then True else False

-- [Pos,Pos,Pos]
-- wenn die Move außer der Board ist , dann wird rekursiv die letzte Position der Line zurückgegeben 
maxmoveslegal :: [Pos] -> Pos -> [Pos]
maxmoveslegal [] a = []
maxmoveslegal (x:xs) a = if isvalidpos x then x : maxmoveslegal xs a else  last(maxmoveslegal (lineminus a x) a) : maxmoveslegal xs a 

-- [Pos,Pos,Pos] -> [line Pos Pos, line Pos Pos, line Pos Pos]
maxmoves :: [Pos] -> Pos -> [Pos]
maxmoves [] a = []
maxmoves (x:xs) a = tail(line a x) ++ maxmoves xs a

moveslistpos :: [Pos] -> Pos -> [Move]
moveslistpos [] a = []
moveslistpos (x:xs) a = if x == a then moveslistpos xs a else (Move a x 0) : moveslistpos xs a


moves :: Pos -> Cell -> [Move]
moves (Pos c r) (Piece p i) = moveslistpos (maxmoves (maxmoveslegal (directionlist (Piece p i) (Pos c r)) (Pos c r)) (Pos c r)) (Pos c r)

           --     | maxmoveslegal (directionlist (Piece p i) (Pos c r)) (Pos c r) == [] = []

rotatation :: Pos -> [Move]
rotatation (Pos c r) = [Move (Pos c r) (Pos c r) 1, Move (Pos c r) (Pos c r) 2, Move (Pos c r) (Pos c r) 3, Move (Pos c r) (Pos c r) 4, Move (Pos c r) (Pos c r) 5, Move (Pos c r) (Pos c r) 6, Move (Pos c r) (Pos c r) 7]

rotatationShields :: Pos -> Pos -> [Move]
rotatationShields (Pos c r) (Pos c1 r1) 
                | (Pos c r) == (Pos c1 r1) = []
                | otherwise = [Move (Pos c r) (Pos c1 r1) 1, Move (Pos c r) (Pos c1 r1) 2, Move (Pos c r) (Pos c1 r1) 3, Move (Pos c r) (Pos c1 r1) 4, Move (Pos c r) (Pos c1 r1) 5, Move (Pos c r) (Pos c1 r1) 6, Move (Pos c r) (Pos c1 r1) 7]

rotatationCommander :: Pos -> [Move]
rotatationCommander (Pos c r)  = [Move (Pos c r) (Pos c r) 1, Move (Pos c r) (Pos c r) 3, Move (Pos c r) (Pos c r) 5, Move (Pos c r) (Pos c r) 7]

rotatationProbe :: Pos -> [Move]
rotatationProbe (Pos c r)  = [Move (Pos c r) (Pos c r) 1, Move (Pos c r) (Pos c r) 2, Move (Pos c r) (Pos c r) 3,  Move (Pos c r) (Pos c r) 5, Move (Pos c r) (Pos c r) 6, Move (Pos c r) (Pos c r) 7]

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves (Pos c r) (Empty) = []
possibleMoves (Pos c r) (Piece p i) 
                | i == 17 || i == 34 || i == 68 || i == 136 = moves (Pos c r) (Piece p i) ++ rotatationProbe (Pos c r)
                | sumofb(inttobinary i) == 1 = moves (Pos c r) (Piece p i) ++ rotatation (Pos c r) ++ rotatationShields (Pos c r) (head(maxmoveslegal (directionlist (Piece p i) (Pos c r)) (Pos c r)))
                | sumofb(inttobinary i) == 4 = moves (Pos c r) (Piece p i) ++ rotatationCommander (Pos c r) 
                | otherwise = moves (Pos c r) (Piece p i) ++ rotatation (Pos c r)


-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Implementation Points                        ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################
--boardtocell :: Board  -> [Cell]
----boardtocell [] = []
--boardtocell (x:xs) = x ++ boardtocell xs

--boardposs :: Board -> [Pos]
--boardposs [] = []
--boardposs (x:xs) = possibleMoves 

pieceblack :: Cell -> Cell
pieceblack (Piece p i) 
                | p == Black = Piece p 1
                | p == White = Piece p 1


listMoves :: Board -> Player -> [Move]
listMoves b p 
                | gameFinished b = []
                | p == White = filter (isValidMove b) (concat[possibleMoves (Pos c r) (b !! (9-r) !! chartonum c) | r <- [1..9], c <- ['a'..'i'], (b !! (9-r) !! chartonum c) /= Empty, pieceblack(b !! (9-r) !! chartonum c) /= Piece Black 1])
                | otherwise = filter (isValidMove b) (concat[possibleMoves (Pos c r) (b !! (9-r) !! chartonum c) | r <- [1..9], c <- ['a'..'i'], (b !! (9-r) !! chartonum c) /= Empty, pieceblack(b !! (9-r) !! chartonum c) /= Piece White 1])


