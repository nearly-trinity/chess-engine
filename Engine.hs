import Data.List
import Data.List.Split
import Data.Maybe

type GameState = (Turn, Won, Board)

getTurn :: GameState -> Turn
getTurn (turn,_,_) = turn
getWon :: GameState -> Won
getWon (_,won,_) = won
getBoard :: GameState -> Board
getBoard (_,_,board) = board

type Won = Bool
data Turn = TColor Color deriving (Show, Eq)
type Board = [Piece]
data Piece = Piece { pColor :: Color,
                     pieceType :: PieceType,
                     loc :: Location }
             deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)
data Location = Location { col :: Int,
                           row :: Int }
                deriving (Show, Eq)

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

-- "5r2/2p2rb1/1pNp4/p2Pp1pk/2P1K3/PP3PP1/5R2/5R2 | w | - | - | 1 | 51"
-- "w" means first elem is 8th row
-- "b" means first elem is 1st row
readBoard :: String -> GameState
readBoard input = let
    (boardData:rest) = splitOn " | " input
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













