import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (intersection, difference, singleton, Set, fromList, member, insert)
import qualified Data.Set as Set

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
             deriving (Eq, Ord)

data Color = Black | White deriving (Show, Eq, Ord)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq, Ord)
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
        in (Just ((colNum, rowNum), piece) : readRow str rowNum (colNum+1))
    | otherwise = error "invalid parse input"

-- "5r2/2p2rb1/1pNp4/p2Pp1pk/2P1K3/PP3PP1/5R2/5R2 | w | - | - | 1 | 51"
-- "w" means first elem is 8th row
-- "b" means first elem is 1st row
startingState = readBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
startingBoard = getBoard startingState


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

----------------------------------------------------------------------------
--                          Legal Moves
---------------------------------------------------------------------------- 

inBounds :: RowNum -> ColNum -> Bool
inBounds row col = row <= 8 && row >= 1 && col <= 8 && col >= 1

lookupLoc :: Board -> Piece -> Maybe Location
lookupLoc [] _ = Nothing
lookupLoc ((loc,piece):pieces) query = 
    if piece == query
    then Just loc
    else lookupLoc pieces query

otherPieces :: Board -> Piece -> [(Location, Piece)]
otherPieces board thisPiece = filter (\(loc, piece) -> piece /= thisPiece) board

negDiagMoves :: (RowNum, ColNum) -> [(RowNum, ColNum)]
negDiagMoves (x,y) = [(x+offset, y-offset) | offset <- [-7..7], inBounds (x+offset) (y-offset)] 

posDiagMoves :: (RowNum, ColNum) -> [(RowNum, ColNum)]
posDiagMoves (x,y) = [(x+offset, y+offset) | offset <- [-7..7], inBounds (x+offset) (y+offset)] 

rowMoves :: (RowNum, ColNum) -> [(RowNum, ColNum)]
rowMoves (x,y) = [(x+offset,y) | offset <- [-7..7], inBounds (x+offset) y]

colMoves :: (RowNum, ColNum) -> [(RowNum, ColNum)]
colMoves (x,y) = [(x,y+offset) | offset <- [-7..7], inBounds x (y+offset)]

-- valid movement for the queen:
--      any y value with eq x
--      any x value with eq y
--      any ((x+a),(y+a)) where x,y is starting pos and a is any constant

testGetMoves = getMoves startingBoard (Piece Black Queen)


getMoves :: Board -> Piece -> Set Location 
getMoves board piece =
    case piece of 
        Piece Black Queen -> let
            (x,y) = fromJust $ lookupLoc board piece
            allMoves = fromList ((rowMoves (x,y)) ++ (colMoves (x,y)) ++ (posDiagMoves (x,y)) ++ (negDiagMoves (x,y)))
            notBlkQueen = fromList $ map fst $ otherPieces board piece
            collisions = allMoves `intersection` notBlkQueen
            -- need helper
            --  for each collision check what quadrant it is relative to black queen
            --  ex. top right, remove moves from "allMoves" that have x > bqX && y > bqY
            --  ex. bottom left, remove moves from "allMoves" that have x < bqX && y < bqY
            in allMoves `difference` notBlkQueen
        x -> error "invalid piece"








        














