import Data.List
import Data.List.Split
import Data.Maybe

type GameState = (Turn, Won, Board)

-- Functions for parsing game state
getTurn :: GameState -> Turn
getTurn (turn,_,_) = turn
getWon :: GameState -> Won
getWon (_,won,_) = won
getBoard :: GameState -> Board
getBoard (_,_,board) = board

-- Whose turn it is (Black or White)
type Won = Bool

-- Won is a bool. True if game is over and false otherwise
data Turn = TColor Color deriving (Show, Eq)

-- Board is a list of pieces
type Board = [Piece]

-- Piece is a structure that has Color (team), PieceType (king, queen, etc), location is rowNum and colNum on the table
data Piece = Piece { pColor :: Color,
                     pType :: PieceType,
                     loc :: Location }
             deriving (Eq)

data Color = Black | White deriving (Show, Eq)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)
data Location = Location { col :: Int,
                           row :: Int }
                deriving (Show, Eq)

-- Capital letters represent pieces of White team while lowercase letters are for Black team
instance Show Piece where
    show (Piece White King _) = "K"
    show (Piece White Queen _) = "Q"
    show (Piece White Rook _) = "R"
    show (Piece White Bishop _) = "B"
    show (Piece White Knight _) = "N"
    show (Piece White Pawn _) = "P"
    show (Piece Black King _) = "k"
    show (Piece Black Queen _) = "q"
    show (Piece Black Rook _) = "r"
    show (Piece Black Bishop _) = "b"
    show (Piece Black Knight _) = "n"
    show (Piece Black Pawn _) = "p"            

type RowNum = Int
type ColNum = Int

readRow :: String -> RowNum -> ColNum -> [Maybe Piece]
readRow _ 9 _ = [] -- edge of the board
readRow [] _ _= [] -- end of string
readRow (char:str) rowNum colNum
    | char >= '1' && char <= '9' = -- blank spaces
        (Nothing : readRow str rowNum (colNum + (read (char:[]) :: Int)))
    | (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') = let -- black / white pieces
        piece = case char of
            'k' -> Just $ Piece Black King (Location colNum rowNum)
            'q' -> Just $ Piece Black Queen (Location colNum rowNum)
            'r' -> Just $ Piece Black Rook (Location colNum rowNum)
            'b' -> Just $ Piece Black Bishop (Location colNum rowNum) 
            'n' -> Just $ Piece Black Knight (Location colNum rowNum)
            'p' -> Just $ Piece Black Pawn (Location colNum rowNum) 
            'K' -> Just $ Piece White King (Location colNum rowNum) 
            'Q' -> Just $ Piece White Queen (Location colNum rowNum)
            'R' -> Just $ Piece White Rook (Location colNum rowNum)
            'B' -> Just $ Piece White Bishop (Location colNum rowNum)
            'N' -> Just $ Piece White Knight (Location colNum rowNum)
            'P' -> Just $ Piece White Pawn (Location colNum rowNum)
            x -> error "invalid piece"
        in (piece : readRow str rowNum (colNum+1))
    | otherwise = error "invalid parse input"

--      Starting Position
-- digit represent number of empty spaces. '/' character separates lines
-- "5r2/2p2rb1/1pNp4/p2Pp1pk/2P1K3/PP3PP1/5R2/5R2 | w | - | - | 1 | 51"
-- "w" means first elem is 8th row
-- "b" means first elem is 1st row
startingPosition = readBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

readBoard :: String -> GameState
readBoard input = let
    (boardData:rest) = splitOn " " input
    turn = case (head rest) of -- this is either "w" or "b"
        "w" -> TColor White
        "b" -> TColor Black
        x -> error "invalid turn"
    board = case (head rest) of
        "w" -> let
            rows = [8,7..1] `zip` (splitOn "/" boardData)
            in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
        "b" -> let
            rows = [1..8] `zip` (splitOn "/" boardData)
            in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
        x -> error "invalid turn"
    in (turn, False, (catMaybes board))













