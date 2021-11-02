
type GameState = (Turn, Won, Board)

getTurn :: GameState -> Turn
getTurn (turn,_,_) = turn
getWon :: GameState -> Won
getWon (_,won,_) = won
getBoard :: GameState -> Board
getBoard (_,_,board) = board

type Won = Bool
data Turn = Color deriving (Show, Eq)
type Board = [Piece]
data Piece = Piece { color :: Color,
                     pieceType :: PieceType,
                     loc :: Location }
             deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)
data PieceType = King | Queen | Rook | DarkBishop | LightBishop | Knight | Pawn deriving (Show, Eq)
data Location = 
    Position { row :: Int,
               col :: Int }
    deriving (Show, Eq)




