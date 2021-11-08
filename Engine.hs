import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set (intersection, difference, singleton, Set, fromList, member, insert)
import qualified Data.Set as Set
import Data.Char (isAsciiLower, isAsciiUpper)
import Text.XHtml.Frameset (cols)

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

startingState = readState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
startingBoard = getBoard startingState

-- parses the FEN notation
readState :: String -> GameState
readState input = let
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
    in (turn, (catMaybes board))

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

-- check if a location is in bounds of the 8x8 grid
inBounds :: (RowNum, ColNum) -> Bool
inBounds (row, col) = row <= 8 && row >= 1 && col <= 8 && col >= 1

-- check if there is a piece at a location on the board
isObstacle :: Board -> (RowNum, ColNum) -> Bool
isObstacle [] _ = False
isObstacle ((loc, _):rest) thisLoc@(x,y) = (x<1 || x>8 || y<1 || y>8) ||
                                            thisLoc == loc || isObstacle rest thisLoc

-- check if a piece is the same team as a piece on a location on the board
isSameColor :: Board -> (RowNum, ColNum) -> Color -> Bool
isSameColor board loc color =
    case lookup loc board of
        Just found -> color == pColor found
        Nothing -> error "invalid location in isSameColor"

-- check if a location is unoccupied
isEmpty :: Board -> (RowNum, ColNum) -> Bool
isEmpty board loc = loc `notElem` map fst board

-- reverse lookUp based on piece value
lookupLoc :: Board -> Piece -> Maybe Location
lookupLoc [] _ = Nothing
lookupLoc ((loc,piece):pieces) query =
    if piece == query
    then Just loc
    else lookupLoc pieces query

-- get a list of pieces other than a particular piece
otherPieces :: Board -> Piece -> [(Location, Piece)]
otherPieces board thisPiece = filter (\(loc, piece) -> piece /= thisPiece) board

-- returns a list of possible moves for a given piece at a given location using a direction lambda
directionalMoves :: ((Int,Int) -> (Int,Int)) -> Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)] -> [(RowNum, ColNum)]
directionalMoves f board loc@(x,y) color moves 
  | inBounds loc =
        if isEmpty board loc then directionalMoves f board (f loc) color (loc:moves)
        else
            if isSameColor board loc color then moves
            else loc:moves
  | otherwise = moves


-- calls directionalMoves to get the list of possible row moves for a given loc
rowMoves :: Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)] -> [(RowNum, ColNum)]
rowMoves board loc@(x,y) color moves  = directionalMoves (\(x,y)->(x+1,y)) board (x+1,y) color [] ++ directionalMoves (\(x,y)->(x-1,y)) board (x-1,y) color [] 

-- calls directionalMoves to get the list of possible column moves for a given loc
colMoves :: Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)] -> [(RowNum, ColNum)]
colMoves board loc@(x,y) color moves  = directionalMoves (\(x,y)->(x,y+1)) board (x,y+1) color [] ++ directionalMoves (\(x,y)->(x,y-1)) board (x,y-1) color [] 

-- calls directionalMoves to get the list of possible diagonal moves for a given loc
diagMoves :: Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)] -> [(RowNum, ColNum)]
diagMoves board loc@(x,y) color moves  = 
    directionalMoves (\(x,y)->(x+1,y+1)) board (x+1,y+1) color [] ++ 
    directionalMoves (\(x,y)->(x-1,y-1)) board (x-1,y-1) color [] ++ 
    directionalMoves (\(x,y)->(x+1,y-1)) board (x+1,y-1) color [] ++ 
    directionalMoves (\(x,y)->(x-1,y+1)) board (x-1,y+1) color []

testGetMoves = getMoves startingBoard (Piece White Queen)

-- gets the list of possible moves for a piece depending on its piece type
getMoves :: Board -> Piece -> [(RowNum, ColNum)]
getMoves board piece = 
    let
        loc@(x,y) = fromJust $ lookupLoc board piece
        color = pColor piece
    in
    case pType piece of
        King -> undefined
        Queen -> rows ++ cols ++ diags
            where 
                rows = rowMoves board loc color []
                cols = colMoves board loc color []
                diags = diagMoves board loc color []
        Rook -> rows ++ cols    
            where
                rows = rowMoves board loc color []
                cols = colMoves board loc color []
        Bishop -> diags
            where
                diags = diagMoves board loc color []
        Knight -> undefined
        Pawn -> undefined    

----------------------------------------------------------------------------
--                          Display Board
---------------------------------------------------------------------------- 

-- Used to offset lines when printing the board
offsetStr = "  " 

-- Returns the concatenation of the strings with a newline added at the end
format :: [String] -> String
format strs = concat $ strs ++ ["\n"]

-- Returns the strings with an offset at the front
offset :: [String] -> [String]
offset strs = [offsetStr] ++ strs

-- Returns a nicely formatted string that represents the given board
-- 
-- Load this file into ghci and enter
testDisplay = putStr $ displayBoard startingBoard
-- To see the output for the starting position

displayBoard :: Board -> String
displayBoard b = makeBorder ++ makeRows b ++ makeLine

-- Creates the line with the labels for the columns of the board
makeBorder :: String
makeBorder = format $ offset $ (map (\x -> " " ++ show x) ['A'..'H'])

-- Creates a string for all of the rows of the board
makeRows:: Board -> String
makeRows b = concat $ (map (\x -> makeRow b x) [8,7..1])

-- Creates a string for a given row of the board
makeRow :: Board -> Int -> String
makeRow b n = format $ [makeLine, show n, " ", makeContent b n]
  
-- Creates a horizontal line
makeLine :: String
makeLine =  format $ offset $ (replicate 8 " ---")

-- Makes the middle of the cells
makeContent :: Board -> Int -> String
makeContent b n = concat $ (map (\x -> "| " ++ makePiece b x ++ " ") [(n,i) | i <- [1..8]]) ++ ["|"]

-- Returns the representation of a piece, or a space if there is no piece at that location
makePiece :: Board -> Location -> String
makePiece b loc =
    case lookupVal loc b of
        Just x -> show x
        Nothing -> " "

-- Just a simple lookup function
lookupVal :: Eq a => a -> [(a, b)] -> Maybe b
lookupVal key lst = lookupHelper ([snd x | x <- lst, key == fst x])

lookupHelper :: [b] -> Maybe b
lookupHelper [x]  = Just x
lookupHelper lst  = Nothing

