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
--     putStr $ displayBoard $ snd startingPosition
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

