import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (intersection, difference, singleton, Set, fromList, member, insert)
import qualified Data.Set as Set
import Data.Char (isAsciiLower, isAsciiUpper)

----------------------------------------------------------------------------
--                          Data Types and Aliases
---------------------------------------------------------------------------- 

type GameState = (Color, Board)

-- bool to determine end game state
type Won = Bool

newtype Turn = TColor Color deriving (Show, Eq)

-- board is a list of location and the piece on that location
type Board = [(Location, Piece)]

-- a piece will have color(black/white), type(king, queen, ... etc)
data Piece = Piece { pColor :: Color,
                     pType :: PieceType }
             deriving (Eq, Ord)

data Color = Black | White deriving (Show, Eq, Ord)
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Show, Eq, Ord)

-- location using a tuple of row and column number
type Location = (Int, Int)

-- uppercase letters are for pieces of white team; lowercase letters are for black team
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

-- matrix notation
type RowNum = Int
type ColNum = Int

----------------------------------------------------------------------------
--                          Board Parser
---------------------------------------------------------------------------- 

-- FEN notation
{- 
    <FEN> ::=  <Piece Placement>
       ' ' <Side to move>
       ' ' <Castling ability>
       ' ' <En passant target square>
       ' ' <Halfmove clock>
       ' ' <Fullmove counter>

-- "5r2/2p2rb1/1pNp4/p2Pp1pk/2P1K3/PP3PP1/5R2/5R2 | w | - | - | 1 | 51"
-- The first field represents the placement of pieces. It starts describing the content of each square, beginning from the eighth rank (lowermost row) and ending with the first (topmost row). For each rank, squares begin from the first file (leftmost column) and go to the eighth (rightmost column).
-- Side to move is one lowercase letter for either White ('w') or Black ('b').
-- If neither side can castle, the symbol '-' is used, otherwise each of four individual castling rights for king and queen castling for both sides are indicated by a sequence of one to four letters.
-- The en passant target square is specified after a double push of a pawn, no matter whether an en passant capture is really possible or not [2] [3] [4] . Other moves than double pawn pushes imply the symbol '-' for this FEN field.
-- The halfmove clock specifies a decimal number of half moves with respect to the 50 move draw rule. It is reset to zero after a capture or a pawn move and incremented otherwise.
-- The number of the full moves in a game. It starts at 1, and is incremented after each Black's move.
-- more info: https://www.chessprogramming.org/Forsyth-Edwards_Notation#Piece_Placement
-}

-- functions to extract turn/board from gameState
getTurn :: GameState -> Color
getTurn (turn,_) = turn
getBoard :: GameState -> Board
getBoard (_,board) = board

-- for debugging purposes
startingState = readBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
startingBoard = getBoard startingState

-- parses the FEN notation
readBoard :: String -> GameState
readBoard input = let
    (boardData:rest) = splitOn " " input
    turn = case head rest of -- this is either "w" or "b"
        "w" -> White
        "b" -> Black
        x -> error "invalid turn"
    board = case head rest of
        "w" -> let
            rows = [8,7..1] `zip` splitOn "/" boardData
            in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
        "b" -> let
            rows = [1..8] `zip` splitOn "/" boardData
            in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
        x -> error "invalid turn"
    in (turn, catMaybes board)

-- helper for readBoard
readRow :: String -> RowNum -> ColNum -> [Maybe (Location, Piece)]
readRow _ 9 _ = [] -- edge of the board
readRow [] _ _= [] -- end of string
readRow (char:str) rowNum colNum
    | char >= '1' && char <= '9' = -- blank spaces
        Nothing : readRow str rowNum (colNum + (read [char] :: Int))
    | isAsciiLower char || isAsciiUpper char = let -- black / white pieces
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
            allMoves = fromList (rowMoves (x,y) ++ colMoves (x,y) ++ posDiagMoves (x,y) ++ negDiagMoves (x,y))
            notBlkQueen = fromList $ map fst $ otherPieces board piece
            collisions = allMoves `intersection` notBlkQueen
            -- need helper
            --  for each collision check what quadrant it is relative to black queen
            --  ex. top right, remove moves from "allMoves" that have x > bqX && y > bqY
            --  ex. bottom left, remove moves from "allMoves" that have x < bqX && y < bqY
            in allMoves `difference` notBlkQueen
        x -> error "invalid piece"























