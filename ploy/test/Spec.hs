-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:test    ##############
-- #############################################################################
import Test.Hspec

import Board
    (   
        validateFEN,
        buildBoard,
        line,
        getCell,
        elembwtilln,
        removenot,
        horizontal,
        vertical,
        diagonal,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos),
       )
import Ploy
    ( 
        inttobinary,
        pieceblack,
        listMoves,
        rotatationCommander,
        directionbin,
        directionbin2,
        directionbin3,
        directionlist,
        moveslistpos,
        moves,
        rotatationShields,
        possibleMoves,
        isValidMove1, 
        blackwayisoccupied,
        whitewayisoccupied,
        blackwayisoccupiedsamecolor,
        whitewayisoccupiedsamecolor,
        rotatation,
        rot1,
        pieceremin,
        remnumofpiece,
        line1free,
        postocells,
        isValidMove,
        checknobwe2,
        checknobwe,
        checknoblack,
        checknowhite,
        checknob,
        checknow,
        gameFinished,
        Move(Move),
        rotate,
        start,
        turn,
        target,
        )


main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testBoard
    testGameFinished
    testPloy
    testIsValidMove
    testPossibleMoves
    testListMoves

sampleBoard :: Board
sampleBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

sampleBoard1 :: Board
sampleBoard1 = [[Empty,Piece White 85,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]


testValidateFEN :: Spec
testValidateFEN = describe "Module Board: validateFEN ..." $ do
        it "fen is empty" $ do
            validateFEN "" `shouldBe` (False :: Bool)
        it "fen has no , and /" $ do
            validateFEN "b24 b43 w221" `shouldBe` (False :: Bool)
        it "fen has no , " $ do
            validateFEN "b24/b43/w221" `shouldBe` (False :: Bool)
        it "fen has no / " $ do
            validateFEN "b24,b43,w221" `shouldBe` (False :: Bool)
        it "fen has pieces with wrong values" $ do
            validateFEN "b2456,b423,b256,w256" `shouldBe` (False :: Bool)
        it "fen has pieces with right values" $ do
            validateFEN ",,b32,,b1,,w255,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)
        it "fen has not 9 rows" $ do
            validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
        it"fen has 9 rows" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)
        it"fen has not 9 rows " $ do
            validateFEN ",,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)
        it"fen columns size not matched " $ do
            validateFEN ",,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
        it"fen contain right elements" $ do
            validateFEN ",,,,b34,,,,/,,,a255,,,,,/,,,,,a1,,,/,,,,,b45,,,/,,,b66,b44,,,,/,,,,,b23,,,/,,,,,,b12,,/,,,,,b66,,,/,,,,a21,,,," `shouldBe` (False :: Bool)
        it"fen contain wrong elements" $ do
            validateFEN ",,,,b34,,,,/,,,a255,,,a,,/,,,z,,fa,,,/,zed,,,,fa,,,/,,,afafs,sdfgx,,,,/,,,,,q23,,,/,,,,,,h12,,/,,,,,b6634,,,/,,,,a2177,,," `shouldBe` (False :: Bool)
        it"elementtilln is false" $ do
            elembwtilln "534" `shouldBe` (False :: Bool)
        it"elementtilln is false" $ do
            elembwtilln "4434" `shouldBe` (False :: Bool)  
        it"elementtilln is true" $ do
            elembwtilln "134" `shouldBe` (True :: Bool) 
        it"elementtilln is false" $ do
            elembwtilln "-2" `shouldBe` (False :: Bool)
        it"elementtilln is false" $ do
            elembwtilln [] `shouldBe` (False :: Bool)
        it "removenot not as expected" $ do
            removenot [",,,,,,,,"] `shouldBe` [",,,,,,,,"]

testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard ..." $ do
        it "cell when empty" $do
            getCell "" `shouldBe` Empty
        it "cell when not black or white" $do
            getCell "a34" `shouldBe` Empty
        it "build empty board" $ do
          buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
        it "build board with pieces" $ do
            buildBoard ",,,,,,b21,,/,,b31,,,,,,/,,,,,,w34,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 21,Empty,Empty],[Empty,Empty,Piece Black 31,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece White 34,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
        it "build board with right pieces " $ do
            buildBoard ",,,,,,b21,,/,,b31,,,,,,/,,,,,,w34,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 21,Empty,Empty],[Empty,Empty,Piece Black 31,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece White 34,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
        it "build board with all pieces " $ do
            buildBoard ",w84,w41,w56,,w56,b21,,/,,b31,,,w170,,,/,,b85,,w1,b1,w34,,/,,b6,w6,,,w12,,/,,,,,,b34,,/,,,,,,w56,,/,,,,,,b78,,/,,,w56,b1,,w90,,/,,w1,w1,w1,b1,,," `shouldBe` [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece Black 21,Empty,Empty],[Empty,Empty,Piece Black 31,Empty,Empty,Piece White 170,Empty,Empty,Empty],[Empty,Empty,Piece Black 85,Empty,Piece White 1,Piece Black 1,Piece White 34,Empty,Empty],[Empty,Empty,Piece Black 6,Piece White 6,Empty,Empty,Piece White 12,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 34,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece White 56,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 78,Empty,Empty],[Empty,Empty,Empty,Piece White 56,Piece Black 1,Empty,Piece White 90,Empty,Empty],[Empty,Empty,Piece White 1,Piece White 1,Piece White 1,Piece Black 1,Empty,Empty,Empty]]
        

testLine :: Spec
testLine = describe "Module Board: line ..." $ do
        
        it "start is target" $ do
            line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])
        it "horizontal move forw" $ do
            line (Pos 'a' 1) (Pos 'a' 2) `shouldBe` ([(Pos 'a' 1),(Pos 'a' 2)] :: [Pos])
        it "horizontal move forw" $ do
            line (Pos 'a' 1) (Pos 'a' 3) `shouldBe` ([(Pos 'a' 1),(Pos 'a' 2),(Pos 'a' 3)] :: [Pos])
        it "horizontal move back" $ do
            line (Pos 'a' 3) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 3),(Pos 'a' 2),(Pos 'a' 1)] :: [Pos])
        it "horizontal not otherwise" $ do
            horizontal (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos]) 
        
        it "vertical move forw" $ do
            line (Pos 'a' 1) (Pos 'b' 1) `shouldBe` ([(Pos 'a' 1),(Pos 'b' 1)] :: [Pos])
        it "vertical move forw" $ do
            line (Pos 'a' 1) (Pos 'c' 1) `shouldBe` ([(Pos 'a' 1),(Pos 'b' 1),(Pos 'c' 1)] :: [Pos])
        it "vertical move back" $ do
            line (Pos 'c' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'c' 1),(Pos 'b' 1),(Pos 'a' 1)] :: [Pos])
        it "vertical not otherwise" $ do
            vertical (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos]) 

        it "diagonal move vert forw" $ do
            line (Pos 'a' 1) (Pos 'b' 2) `shouldBe` ([(Pos 'a' 1),(Pos 'b' 2)] :: [Pos])
        it "diagonal move vert forw" $ do
            line (Pos 'a' 1) (Pos 'c' 3) `shouldBe` ([(Pos 'a' 1),(Pos 'b' 2),(Pos 'c' 3)] :: [Pos])
        it "diagonal move vert back" $ do
            line (Pos 'c' 3) (Pos 'a' 1) `shouldBe` ([(Pos 'c' 3),(Pos 'b' 2),(Pos 'a' 1)] :: [Pos])
        it "diagonal move hor forw" $ do
            line (Pos 'f' 5)(Pos 'd' 7) `shouldBe` ([(Pos 'f' 5),(Pos 'e' 6),(Pos 'd' 7)] :: [Pos])
        it "diagonal move hor back" $ do
            line (Pos 'd' 7) (Pos 'f' 5) `shouldBe` ([(Pos 'd' 7),(Pos 'e' 6),(Pos 'f' 5)] :: [Pos])
        it "diagonal not otherwise" $ do
            diagonal (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])
        it "diagonal otherwise" $ do
            diagonal (Pos 'a' 1) (Pos 'b' 2) `shouldBe` ([(Pos 'a' 1),(Pos 'b' 2)] :: [Pos])
        it "diagonal otherwise" $ do
            diagonal (Pos 'b' 2) (Pos 'a' 1) `shouldBe` ([(Pos 'b' 2),(Pos 'a' 1)] :: [Pos])
        it "vertical otherwise" $ do
            vertical (Pos 'a' 2) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 2),(Pos 'a' 1)] :: [Pos])
        it "vertical not otherwise" $ do
            vertical (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])
        it "horizontal otherwise" $ do
            horizontal (Pos 'b' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'b' 1),(Pos 'a' 1)] :: [Pos])



testBoard :: Spec
testBoard = describe "Module Board: board ..." $ do
            it  "Module Board: player ..." $ do
               (Piece White 1) == Empty `shouldBe` (False :: Bool)
            it  "Module Board: player ..." $ do
               Black == White `shouldBe` (False :: Bool)
            it  "show ..." $ do
                show (Piece White 1) `shouldBe` "Piece White 1"
            it  "show ..." $ do
                show  ((Pos 'a' 1)) `shouldBe` "Pos {col = 'a', row = 1}"
            it  "show ..." $ do
                show  ( Black ) `shouldBe` "Black"

testPloy :: Spec
testPloy = describe "Module Board: ploy ..." $ do
            it  "move ..." $ do
                show (Move (Pos 'a' 1) (Pos 'a' 2)1) `shouldBe` "a1-a2-1"
            it  "move ..." $ do
                Move (Pos 'a' 1) (Pos 'a' 2)2 `shouldBe` (Move (Pos 'a' 1) (Pos 'a' 2)2 :: Move)
            it  "move ..." $ do
                Move (Pos 'a' 1) (Pos 'a' 1)1 == Move (Pos 'a' 1) (Pos 'a' 1)1 `shouldBe` (True :: Bool)
            it  "rotate ..." $ do
                rotate 1 1 `shouldBe` (2 :: Int)
            it "move start ..." $ do
                show (start (Move (Pos 'a' 1) (Pos 'a' 2)1)) `shouldBe` "Pos {col = 'a', row = 1}"
            it "move target ..." $ do
                show (target (Move (Pos 'a' 1) (Pos 'a' 2)1)) `shouldBe` "Pos {col = 'a', row = 2}"
            it "move turn ..." $ do
                turn (Move (Pos 'a' 1) (Pos 'a' 2)1) `shouldBe` (1 :: Int)
            
            
testGameFinished :: Spec
testGameFinished = describe "Module Board: gameFinished ..." $ do
            it  "gameFinished ..." $ do
                gameFinished sampleBoard `shouldBe` (True :: Bool)
            it  "not gameFinished ..." $ do
                gameFinished sampleBoard1 `shouldBe` (False :: Bool)
            it "check now... " $ do
                checknow [] `shouldBe` (True :: Bool)
            it "checnowhite ..." $ do
                checknowhite [] `shouldBe` (True :: Bool)
            it "checknob ..." $ do
                checknob [] `shouldBe` (True :: Bool)
            it "checknob ..." $ do
                checknob [Piece Black 85] `shouldBe` (False :: Bool)
            it "checknob ..." $ do
                checknob [] `shouldBe` (True :: Bool)
            it "checknob ..." $ do
                checknob [Piece Black 1] `shouldBe` (True :: Bool)
            it "checknoblack empty ..." $ do
                checknoblack [] `shouldBe` (True :: Bool)
            it "checknobwe empty ..." $ do
                checknobwe [] `shouldBe` (True :: Bool)
            it "checknobwe2 empty ..." $ do 
                checknobwe2 [] `shouldBe` (True :: Bool)

sampleBoard2 :: Board
sampleBoard2 = [[Empty,Piece Black 85,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

sampleBoard3 :: Board
sampleBoard3 = [[Empty,Piece Black 85,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece White 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

sampleBoard4 :: Board
sampleBoard4 = [[Empty,Piece Black 85,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 85,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece White 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]


testIsValidMove :: Spec
testIsValidMove = describe "Module Board: isValidMove ..." $ do
            it  "isValidMove ..." $ do
                isValidMove sampleBoard2 (Move (Pos 'b' 9) (Pos 'c' 9)0) `shouldBe` (True :: Bool)
            it  "isValidMove ..." $ do
                isValidMove sampleBoard (Move (Pos 'b' 9) (Pos 'a' 9)0) `shouldBe` (True :: Bool)
            it  "isValidMove ..." $ do
                isValidMove sampleBoard (Move (Pos 'b' 9) (Pos 'b' 8)0) `shouldBe` (True :: Bool)
            it  "isValidMove ..." $ do
                isValidMove sampleBoard (Move (Pos 'b' 9) (Pos 'd' 9)0) `shouldBe` (False :: Bool)
            it "line1free ..." $ do
                line1free [(Pos 'a' 9), (Pos 'b' 9)] sampleBoard2  `shouldBe` ((Piece Black 85) :: Cell)
            it "postocells ..." $ do
                postocells sampleBoard2 [(Pos 'a' 9),(Pos 'b' 9)] `shouldBe` ([Empty , (Piece Black 85)] :: [Cell])
            it "rennum ... " $ do
                remnumofpiece Empty `shouldBe` (Empty :: Cell)
            it "pieceremin ..." $ do    
                pieceremin [] `shouldBe` ([] :: [Cell])
            it "pieceremin ..." $ do    
                pieceremin [(Piece White 1) ,(Piece White 2)] `shouldBe` ([(Piece White 1) ,(Piece White 1)] :: [Cell])
            it "blackwayisoccupied ..." $ do
                blackwayisoccupied sampleBoard2 (Move (Pos 'b' 1) (Pos 'h' 1)0) `shouldBe` (True :: Bool)
            it "blackwayisoccupied ..." $ do
                blackwayisoccupied sampleBoard2 (Move (Pos 'b' 9) (Pos 'h' 9)0) `shouldBe` (True :: Bool)
            it "whitewayisoccupied ..." $ do
                whitewayisoccupied sampleBoard3 (Move (Pos 'b' 1) (Pos 'h' 1)0) `shouldBe` (True :: Bool)
            it "whitewayisoccupied ..." $ do
                whitewayisoccupied sampleBoard3 (Move (Pos 'b' 1) (Pos 'a' 1)0) `shouldBe` (False :: Bool)
            it "whitewayisoccupied ..." $ do
                whitewayisoccupied sampleBoard3 (Move (Pos 'c' 9) (Pos 'h' 9)0) `shouldBe` (True :: Bool)
            it "blackway ..." $ do
                blackwayisoccupiedsamecolor sampleBoard3 (Move (Pos 'c' 1) (Pos 'h' 1)0) `shouldBe` (True :: Bool)
            it "whiteway ..." $ do
                whitewayisoccupiedsamecolor sampleBoard3 (Move (Pos 'c' 9) (Pos 'h' 9)0) `shouldBe` (True :: Bool)
            it "whiteway ..." $ do
                whitewayisoccupiedsamecolor sampleBoard3 (Move (Pos 'c' 1) (Pos 'h' 1)0) `shouldBe` (False :: Bool)
            it "rot1..." $ do
                rot1 sampleBoard3 (Move (Pos 'c' 1) (Pos 'h' 1)0) `shouldBe` (False :: Bool)
            it "rot1..." $ do
                rot1 sampleBoard3 (Move (Pos 'c' 1) (Pos 'c' 1)1) `shouldBe` (True :: Bool)
            it "rot1..." $ do
                rot1 sampleBoard3 (Move (Pos 'b' 1) (Pos 'a' 1)1) `shouldBe` (True :: Bool)
            it "isValidMove1..." $ do
                isValidMove1 sampleBoard3 (Move (Pos 'a' 9) (Pos 'a' 1)0) `shouldBe` (False :: Bool)
            it "isValidMove1..." $ do
                isValidMove1 sampleBoard3 (Move (Pos 'b' 9) (Pos 'a' 9)1) `shouldBe` (True :: Bool)
            it "blackwayisoccupied ..." $ do
                isValidMove1 sampleBoard2 (Move (Pos 'b' 1) (Pos 'h' 1)0) `shouldBe` (False :: Bool)
            it "whitewayisoccupied ..." $ do
                isValidMove1 sampleBoard3 (Move (Pos 'b' 1) (Pos 'h' 1)0) `shouldBe` (False :: Bool)
            it "whitewayisoccupied ..." $ do
                isValidMove1 sampleBoard3 (Move (Pos 'c' 9) (Pos 'h' 9)0) `shouldBe` (False :: Bool)
            it "blackway ..." $ do
                isValidMove1 sampleBoard3 (Move (Pos 'c' 1) (Pos 'h' 1)0) `shouldBe` (False :: Bool)
            it "blackwayisocc ..." $ do
                isValidMove1 sampleBoard3 (Move (Pos 'b' 9) (Pos 'h' 9)1) `shouldBe` (False :: Bool)
            it "validmove ... " $ do
                isValidMove sampleBoard3 (Move (Pos 'b' 9) (Pos 'h' 9)1) `shouldBe` (False :: Bool)
            it "rot1..." $ do
                show (rot1 sampleBoard3 (Move (Pos 'c' 1) (Pos 'h' 1)1)) `shouldBe` ("False" :: String)
            it "isValidMove... show rotation" $ do
                show (isValidMove sampleBoard3 (Move (Pos 'c' 1) (Pos 'h' 1)1)) `shouldBe` ("False" :: String)

testPossibleMoves :: Spec
testPossibleMoves = describe "Module Board: possibleMoves ..." $ do
            it  "possibleMoves ..." $ do
                possibleMoves (Pos 'b' 2) (Empty) `shouldBe` ([] :: [Move])
            it  "possibleMoves ..." $ do
                possibleMoves (Pos 'b' 2) (Piece White 1) `shouldBe` ([(Move (Pos 'b' 2) (Pos 'b' 3) 0),(Move (Pos 'b' 2) (Pos 'b' 2) 1),(Move (Pos 'b' 2) (Pos 'b' 2) 2), (Move (Pos 'b' 2) (Pos 'b' 2) 3) , (Move (Pos 'b' 2) (Pos 'b' 2) 4) , (Move (Pos 'b' 2) (Pos 'b' 2) 5), (Move (Pos 'b' 2) (Pos 'b' 2) 6), (Move (Pos 'b' 2) (Pos 'b' 2) 7),(Move (Pos 'b' 2) (Pos 'b' 3) 1),(Move (Pos 'b' 2) (Pos 'b' 3) 2), (Move (Pos 'b' 2) (Pos 'b' 3) 3) , (Move (Pos 'b' 2) (Pos 'b' 3) 4) , (Move (Pos 'b' 2) (Pos 'b' 3) 5), (Move (Pos 'b' 2) (Pos 'b' 3) 6), (Move (Pos 'b' 2) (Pos 'b' 3) 7) ] :: [Move])
            it "possibleMoves ..." $ do
                possibleMoves (Pos 'b' 9) (Piece Black 16) `shouldBe` ([(Move (Pos 'b' 9) (Pos 'b' 8) 0),(Move (Pos 'b' 9) (Pos 'b' 9) 1),(Move (Pos 'b' 9) (Pos 'b' 9) 2), (Move (Pos 'b' 9) (Pos 'b' 9) 3) , (Move (Pos 'b' 9) (Pos 'b' 9) 4) , (Move (Pos 'b' 9) (Pos 'b' 9) 5), (Move (Pos 'b' 9) (Pos 'b' 9) 6), (Move (Pos 'b' 9) (Pos 'b' 9) 7),(Move (Pos 'b' 9) (Pos 'b' 8) 1),(Move (Pos 'b' 9) (Pos 'b' 8) 2), (Move (Pos 'b' 9) (Pos 'b' 8) 3) , (Move (Pos 'b' 9) (Pos 'b' 8) 4) , (Move (Pos 'b' 9) (Pos 'b' 8) 5), (Move (Pos 'b' 9) (Pos 'b' 8) 6), (Move (Pos 'b' 9) (Pos 'b' 8) 7) ] :: [Move])
            it "possibleMoves ..." $ do
                possibleMoves (Pos 'b' 9) (Piece Black 3) `shouldBe` ([(Move (Pos 'b' 9) (Pos 'b' 9) 1),(Move (Pos 'b' 9) (Pos 'b' 9) 2),(Move (Pos 'b' 9) (Pos 'b' 9) 3),(Move (Pos 'b' 9) (Pos 'b' 9) 4),(Move (Pos 'b' 9) (Pos 'b' 9) 5),(Move (Pos 'b' 9) (Pos 'b' 9) 6),(Move (Pos 'b' 9) (Pos 'b' 9) 7) ] :: [Move])
            it "possibleMoves ..." $ do
                possibleMoves (Pos 'b' 9) (Piece White 24) `shouldContain` ([(Move (Pos 'b' 9) (Pos 'b' 8) 0), (Move (Pos 'b' 9) (Pos 'b' 7) 0), (Move (Pos 'b' 9) (Pos 'c' 8) 0), (Move (Pos 'b' 9) (Pos 'd' 7) 0) ,(Move (Pos 'b' 9) (Pos 'b' 9) 1),(Move (Pos 'b' 9) (Pos 'b' 9) 2),(Move (Pos 'b' 9) (Pos 'b' 9) 3),(Move (Pos 'b' 9) (Pos 'b' 9) 4),(Move (Pos 'b' 9) (Pos 'b' 9) 5),(Move (Pos 'b' 9) (Pos 'b' 9) 6),(Move (Pos 'b' 9) (Pos 'b' 9) 7)   ] :: [Move])
            it "rotationsh ..." $ do
                rotatationShields (Pos 'b' 2) (Pos 'b' 2) `shouldBe` ([] :: [Move])
            it "moves ..." $ do
                moves (Pos 'a' 9) (Piece White 128) `shouldBe` ([] :: [Move])
            it "moveslistpos ..." $ do
                moveslistpos [(Pos 'a' 9)] (Pos 'a' 9)  `shouldBe` ([] :: [Move])
            it "directionlist ..." $ do
                directionlist (Piece White 85) (Pos 'b' 4)   `shouldContain` ([(Pos 'c' 4)] :: [Pos])
            it "directionlist ..." $ do
                directionlist (Piece White 7) (Pos 'b' 4)   `shouldContain` ([(Pos 'e' 7)] :: [Pos])
            it "directionbin..." $ do
                directionbin (Piece White 2) (Pos 'b' 4)   `shouldBe` ((Pos 'c' 5) :: Pos)
            it "directionbin..." $ do
                directionbin (Piece White 8) (Pos 'b' 4)   `shouldBe` ((Pos 'c' 3) :: Pos)
            it "directionbin..." $ do
                directionbin (Piece White 32) (Pos 'b' 4)   `shouldBe` ((Pos 'a' 3) :: Pos)
            it "directionbin..." $ do
                directionbin (Piece White 64) (Pos 'b' 4)   `shouldBe` ((Pos 'a' 4) :: Pos)
            it "directionbin..." $ do
                directionbin2 (Piece White 1) (Pos 'b' 4)   `shouldBe` ((Pos 'b' 6) :: Pos)
            it "directionbin..." $ do
                directionbin2 (Piece White 4) (Pos 'b' 4)   `shouldBe` ((Pos 'd' 4) :: Pos)
            it "directionbin..." $ do
                directionbin2 (Piece White 32) (Pos 'c' 4)   `shouldBe` ((Pos 'a' 2) :: Pos)
            it "directionbin..." $ do
                directionbin2 (Piece White 64) (Pos 'c' 4)   `shouldBe` ((Pos 'a' 4) :: Pos)
            it "directionbin..." $ do
                directionbin2 (Piece White 128) (Pos 'c' 4)   `shouldBe` ((Pos 'a' 6) :: Pos)
            it "directionbin..." $ do
                directionbin3 (Piece White 1) (Pos 'b' 4)   `shouldBe` ((Pos 'b' 7) :: Pos)
            it "directionbin..." $ do
                directionbin3 (Piece White 8) (Pos 'd' 4)   `shouldBe` ((Pos 'g' 1) :: Pos)
            it "directionbin..." $ do
                directionbin3 (Piece White 16) (Pos 'c' 4)   `shouldBe` ((Pos 'c' 1) :: Pos)
            it "directionbin..." $ do
                directionbin3 (Piece White 32) (Pos 'd' 4)   `shouldBe` ((Pos 'a' 1) :: Pos)
            it "directionbin..." $ do
                directionbin3 (Piece White 64) (Pos 'd' 4)   `shouldBe` ((Pos 'a' 4) :: Pos)
            it "directionbin..." $ do
                directionbin3 (Piece White 128) (Pos 'd' 4)   `shouldBe` ((Pos 'a' 7) :: Pos)
            it "rot commm" $ do
                rotatationCommander (Pos 'b' 4)  `shouldBe` ([(Move (Pos 'b' 4) (Pos 'b' 4) 1),(Move (Pos 'b' 4) (Pos 'b' 4) 3),(Move (Pos 'b' 4) (Pos 'b' 4) 5),(Move (Pos 'b' 4) (Pos 'b' 4) 7)] :: [Move])
            it "possiblemove comm ..." $ do
                possibleMoves (Pos 'b' 4) (Piece White 85) `shouldContain` ([(Move (Pos 'b' 4) (Pos 'b' 4) 5)] :: [Move])
            it "inttobinary..." $ do
                inttobinary 0   `shouldBe` ([] :: [Int])
            it "possiblemoves..." $ do
                show (Black)  `shouldBe` ("Black" :: String)
            it "possiblemoves..." $ do
                show (White)  `shouldBe` ("White" :: String)
            it "possiblemoves..." $ do
                show (Pos 'a' 1)  `shouldBe` ("Pos {col = 'a', row = 1}" :: String)
            it "possiblemoves..." $ do
                show (Piece White 1)  `shouldBe` ("Piece White 1" :: String)

testListMoves :: Spec
testListMoves = describe "listMoves" $ do
        it "listMoves ..." $ do
            listMoves sampleBoard4 White `shouldContain` ([(Move (Pos 'c' 9) (Pos 'c' 9) 1)] :: [Move])
        it "listMoves ..." $ do
            listMoves sampleBoard4 Black `shouldContain` ([(Move (Pos 'b' 9) (Pos 'b' 9) 1)] :: [Move])
        it "listMoves ..." $ do
            listMoves sampleBoard2 Black `shouldBe` ([] :: [Move])
        

        
