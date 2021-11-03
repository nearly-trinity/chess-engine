import Data.List
import Data.List.Split
import Data.Maybe

type GameState = (Color, Board)

getTurn :: GameState -> Color
getTurn (turn,_) = turn
getBoard :: GameState -> Board
getBoard (_,board) = board

type Won = Bool
data Turn = TColor Color deriving (Show, Eq)
type Board = [(Location, Piece)]
data Piece = Piece { pColor :: Color,
                     pType :: PieceType }
             deriving (Eq)

data Color = Black | White deriving (Show, Eq)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq)
type Location = (Int, Int)

-- Capital letters represent pieces of White team while lowercase letters are for Black team
instance Show Piece where
    show (Piece White King) = "K"
    show (Piece White Queen) = "Q"
    show (Piece White Rook) = "R"
    show (Piece White Bishop) = "B"
    show (Piece White Knight) = "N"
    show (Piece White Pawn) = "P"
    show (Piece Black King) = "k"
    show (Piece Black Queen) = "q"
    show (Piece Black Rook) = "r"
    show (Piece Black Bishop) = "b"
    show (Piece Black Knight) = "n"
    show (Piece Black Pawn) = "p"            

type RowNum = Int
type ColNum = Int

readRow :: String -> RowNum -> ColNum -> [Maybe (Location, Piece)]
readRow _ 9 _ = [] -- edge of the board
readRow [] _ _= [] -- end of string
readRow (char:str) rowNum colNum
    | char >= '1' && char <= '9' = -- blank spaces
        (Nothing : readRow str rowNum (colNum + (read (char:[]) :: Int)))
    | (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') = let -- black / white pieces
        piece = case char of
            'k' -> Piece Black King 
            'q' -> Piece Black Queen 
            'r' -> Piece Black Rook 
            'b' -> Piece Black Bishop  
            'n' -> Piece Black Knight 
            'p' -> Piece Black Pawn  
            'K' -> Piece White King  
            'Q' -> Piece White Queen 
            'R' -> Piece White Rook 
            'B' -> Piece White Bishop 
            'N' -> Piece White Knight 
            'P' -> Piece White Pawn 
            x -> error "invalid piece"
        in (Just ((rowNum, colNum), piece) : readRow str rowNum (colNum+1))
    | otherwise = error "invalid parse input"

-- "5r2/2p2rb1/1pNp4/p2Pp1pk/2P1K3/PP3PP1/5R2/5R2 | w | - | - | 1 | 51"
-- "w" means first elem is 8th row
-- "b" means first elem is 1st row
startingPosition = readBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

readBoard :: String -> GameState
readBoard input = let
    (boardData:rest) = splitOn " " input
    turn = case (head rest) of -- this is either "w" or "b"
        "w" -> White
        "b" -> Black
        x -> error "invalid turn"
    board = case (head rest) of
        "w" -> let
            rows = [8,7..1] `zip` (splitOn "/" boardData)
            in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
        "b" -> let
            rows = [1..8] `zip` (splitOn "/" boardData)
            in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
        x -> error "invalid turn"
    in (turn, (catMaybes board))

{-
findPieceAtLoc :: Board -> ColNum -> RowNum -> Maybe Piece
findPieceAtLoc pieces col row = let
    searchLoc = Location col row
    foundPiece = filter (\(x) -> loc x == searchLoc) pieces
    in if(null foundPiece) 
    then Nothing
    else Just $ (head foundPiece)

scanCols :: Board -> ColNum -> RowNum -> String
scanCols pieces col row = let
    found = findPieceAtLoc pieces col row
    in if isNothing found
    then "  "
    else show found ++ " "

scanRows :: Board -> RowNum -> String
scanRows pieces 8 = scanCols 1 8
scanRows pieces row = scanCols 1 row 

displayBoard :: Board -> String
displayBoard pieces = scanRows pieces 1
-}








 
