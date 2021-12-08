module Engine where

import Data.List ( partition )
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, fromJust, isJust, isNothing )
import Data.Char ( digitToInt, chr, isAsciiLower, isAsciiUpper )



----------------------------------------------------------------------------
--                          Data Types and Aliases
---------------------------------------------------------------------------- 

type GameState = (Color, Board, Turns)

-- bool to determine end game state
data Outcome = Win Color | Tie deriving (Show, Eq)

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

-- matrix notation
type RowNum = Int
type ColNum = Int
type Turns = Int


type Move = (Location, Piece)
type PieceLocation = (Location, Piece)

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
-- -- more info: https://www.chessprogramming.org/Forsyth-Edwards_Notation#Piece_Placement
-}

-- functions to extract turn/board from gameState
getTurn :: GameState -> Color
getTurn (turn,_,_) = turn
getBoard :: GameState -> Board
getBoard (_,board,_) = board
getNumTurns :: GameState -> Turns
getNumTurns (_,_,turns) = turns

prettyBoard :: Board -> [((Char, RowNum), Piece)]
prettyBoard = map (\((col,row), piece) -> ((chr (64+col),row), piece))


-- blackTurnState = readState "r3kb1r/ppp3pp/5p2/3p1b2/6P1/4P3/P2NBPP1/3K3R b kq - 0 16"
-- parses the FEN notation
readState :: String -> GameState
readState input = 
  let (boardData:rest) = splitOn " " (filter (/='\n') input)
      turn = case head rest of -- this is either "w" or "b"
          "w" -> White
          "b" -> Black
          x -> error "invalid turn"
      board = let
          rows = [8,7..1] `zip` splitOn "/" boardData
          in concat [readRow str rowNum 1 | (rowNum, str) <- rows]
      numTurns = makeInt (last rest) (length $ last rest)
      in (turn, catMaybes board, numTurns)
      where
          makeInt :: [Char] -> Int -> Int
          makeInt [] _ = 0
          makeInt (x:xs) ind = digitToInt x * (10^(ind-1)) + makeInt xs (ind-1)

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

-- check if a piece is the same team as a piece on a location on the board
isSameColor :: Board -> (RowNum, ColNum) -> Color -> Bool
isSameColor board loc color =
    case lookup loc board of
        Just found -> color == pColor found
        Nothing -> error "invalid location in isSameColor"

-- check if a location is unoccupied
isEmpty :: Board -> (RowNum, ColNum) -> Bool
isEmpty board loc = loc `notElem` map fst board

isCap board ((_,color), loc) =
    case lookup loc board of
              Nothing -> False
              Just (_, pcolor) -> color /= pcolor

-- reverse lookUp based on piece value
lookupLoc :: Board -> Piece -> Maybe Location
lookupLoc [] _ = Nothing
lookupLoc ((loc,piece):pieces) query =
    if piece == query
    then Just loc
    else lookupLoc pieces query

-- returns a list of possible moves for a given piece at a given location using a direction offset
directionalMoves :: [(Int, Int)] -> Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)]
directionalMoves directions board loc color = concat [aux f (f loc) | f <- moveFuns]
   where
      moveFuns = [ (\(x,y) -> (x+dx, y+dy)) | (dx,dy) <- directions]
      aux f loc
         | inBounds loc && isEmpty board loc = loc:aux f (f loc)
         | inBounds loc && not (isSameColor board loc color) = [loc]
         | otherwise = []

-- returns a bool that represents if a move to a certain location on a board is permisilbe
shouldMove :: Board -> (RowNum, ColNum) -> Color -> Bool
shouldMove board loc color
    | inBounds loc =
        isEmpty board loc || not (isSameColor board loc color)
    | otherwise = False

-- calls directionalMoves to get the list of possible row moves for a given loc
rowMoves :: Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)]
rowMoves board loc@(x,y) = directionalMoves [(1,0), (-1,0)] board loc

-- calls directionalMoves to get the list of possible column moves for a given loc
colMoves :: Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)]
colMoves board loc@(x,y) = directionalMoves [(0,1), (0,-1)] board loc

-- -- calls directionalMoves to get the list of possible diagonal moves for a given loc
diagMoves :: Board -> (RowNum, ColNum) -> Color -> [(RowNum, ColNum)]
diagMoves board loc@(x,y) = directionalMoves [(1, 1), (-1, -1), (1, -1), (-1, 1)] board loc

pawnMove :: Board -> (ColNum , RowNum) -> Color -> [(ColNum, RowNum)]
pawnMove board loc@(col,row) color = let
    -- for normal movement of pawn piece
    moveSquares = case color of
        Black -> -- if row number is 7 then we can move twice
            pawnAdvance (\(col, row) -> (col, row-1)) (\(col, row) -> (col, row-2)) 7
        White -> -- if row number is 2 then we can move twice
            pawnAdvance (\(col, row) -> (col, row+1)) (\(col, row) -> (col, row+2)) 2
    -- for when pawn can capture an enemy piece diagonally 
    captureSquares = case color of
        Black ->
            pawnCapture (\(col, row) -> (col-1, row-1)) (\(col, row) -> (col+1, row-1))
        White ->
            pawnCapture (\(col, row) -> (col-1, row+1)) (\(col, row) -> (col+1, row+1))
    in moveSquares ++ captureSquares
    where
        pawnAdvance f1 f2 strtRow =
            let oneAdvance = lookup (f1 loc) board
                twoAdvance = lookup (f2 loc) board
            in  if isNothing oneAdvance && row == strtRow && isNothing twoAdvance
                then [f1 loc, f2 loc] else
                [f1 loc | isNothing oneAdvance]
        pawnCapture f1 f2 =
            let leftAdvance  = lookup (f1 loc) board
                rightAdvance = lookup (f2 loc) board
                leftCap = case leftAdvance of
                    Nothing -> False
                    Just x -> pColor x /= color
                rightCap = case rightAdvance of
                    Nothing -> False
                    Just x -> pColor x /= color
            in  if leftCap && rightCap
                    then [f1 loc, f2 loc] else
                if leftCap then [f1 loc] else
                    [f2 loc | rightCap]

-- gets the list of possible moves for a piece depending on its piece type
getMoves :: GameState -> (Location, Piece) -> [(Location, Piece)]
getMoves (turn, board, 0) (loc, piece) = []
getMoves (turn, board, _) (l@(x,y), piece) = [(loc, piece) | loc <- aux rank]
    where
          col = pColor piece
          rank = pType piece
          goodMove p = shouldMove board p col
          aux King = filter goodMove [(x+1,y+1),(x+1,y-1), (x-1,y+1), (x-1,y-1), (x,y-1), (x-1,y), (x+1,y), (x,y+1)]
          aux Knight = filter goodMove [(x+1,y+2),(x+2,y+1), (x-1,y+2), (x-2,y+1), (x-1,y-2), (x-2,y-1), (x+1,y-2), (x+2,y-1)]
          aux Queen = rowMoves board l col ++ colMoves board l col ++ diagMoves board l col
          aux Rook = rowMoves board l col ++ colMoves board l col
          aux Bishop = diagMoves board l col
          aux Pawn = pawnMove board l col

-- converts col numbers to letters like a typical chess board
prettyMoves :: [(ColNum, RowNum)] -> [(Char, RowNum)]
prettyMoves = map (\(col, row) -> (chr (64 + col), row))

------------------------------------------------------------------
--                      Game Engine
------------------------------------------------------------------

opColor :: Color -> Color
opColor Black = White
opColor White = Black

makeMove :: GameState -> (Location, Piece) -> Location -> GameState
makeMove (turn, board, turns) (from, piece) to = let
    color = pColor piece
    capPiece = (to, fromJust $ lookup to board)
    capBoard = filter (\p -> p /= (from,piece) && p /= capPiece) board
    remBoard = filter (/= (from, piece)) board
    possibleMoves = map fst $ getMoves (turn, board, turns) (from, piece)
    in
        if to `elem` possibleMoves && color == turn then
            if isJust (lookup to board) -- if we need to caputure a piece
                then (opColor color, (to, piece):capBoard, turns - 1)
            else (opColor color, (to, piece):remBoard, turns - 1)
        else error "invalid move"
        
unsafeMakeMove :: GameState -> (Location, Piece) -> Location -> GameState
unsafeMakeMove state@(turn,bd,turns) start@(from, piece) to = let
    newBoard = filter (\p@(loc,_) -> loc /= to && p /= start) bd
    in (inverse turn, (to,piece):newBoard, turns-1)
    




isWinner :: GameState-> Maybe Outcome
isWinner (_, board, turns) =
     let res = playerWinner board
     in if(res == Nothing && turns == 0) then Just Tie else res

playerWinner :: Board -> Maybe Outcome
playerWinner board = let pieces = [piece | (loc,piece) <- board]
                 in
                     if elem (Piece Black King) pieces && notElem (Piece White King) pieces
                         then Just (Win Black)
                     else if elem (Piece White King) pieces && notElem (Piece Black King) pieces
                         then Just (Win White)
                     else if notElem (Piece White King) pieces && notElem (Piece Black King) pieces
                         then error "invalid board: both kings do not exist"
                     else Nothing

type EvalScore = Double
type ColoredPieces = [(Location, Piece)]

(whitePos, blackPos) = partition (\(loc, p) -> pColor p == White) blackTurnBoard

mobilityScore :: GameState -> ColoredPieces -> Int
mobilityScore state pieces = let
    allMoves = [getMoves state piece | piece <- pieces]
    in length $ concat allMoves

materialScore :: ColoredPieces -> Int
materialScore [] = 0
materialScore ((loc,p):ps) = case pType p of
    King -> 100 + materialScore ps
    Queen -> 9 + materialScore ps
    Rook -> 5 + materialScore ps
    Bishop -> 3 + materialScore ps
    Knight -> 3 + materialScore ps
    Pawn -> 1 + materialScore ps

eval :: GameState -> EvalScore
eval (turn, board, x) = let
    (whitePos, blackPos) = partition (\(loc,p) -> pColor p == White) board
    matScore = materialScore whitePos - materialScore blackPos
    whiteMobile = mobilityScore (turn,board, x) whitePos
    blackMobile = mobilityScore (turn,board, x) blackPos
    --mobScore = fromIntegral (whiteMobile - blackMobile) / 5
    in fromIntegral matScore -- + mobScore


-- black eval is (-), white eval is (+), thus if black is winning the evaluation will be negative
-- use some evaluation funtion to calculate the position

---------------------------------------------
--               Best Play
-------------------------------------------

maxDepth = 3
-- generate the game tree by evaluating every possible move for every piece
-- generate all moves: [getMoves GameState piece | piece <- All Pieces]
-- for each move in all moves, call makeMove to return the updated GameState and do this recursively
-- p :: (Location, Piece)
-- allMoves :: list of tuple of piece that is being moves and all of its possible moves

statesForPiece :: GameState -> PieceLocation -> [Move] -> [(Move, GameState)]
statesForPiece state from@(loc, piece) moves = [(move, makeMove state from to) | move@(to,p) <- moves]

whoWillWin :: GameState -> Outcome
whoWillWin (col, board, turns) = 
    case isWinner (col, board, turns) of 
        Just x -> x
        Nothing -> 
            let res = map (\gs -> whoWillWin gs) (allNextStates (col,board,turns))
            in if(Win col `elem` res) then Win col 
               else if(Tie `elem` res) then Tie 
               else Win (inverse col)

inverse :: Color -> Color
inverse White = Black
inverse Black = White

bestMove :: GameState -> Move
-- pass along a boolean to determine turn, if its starting p's turn then condition is any
-- if its other p's turn then its all states leading to checkmate for first player
bestMove state@(turn,board,_) = let
    startingColor = turn
    allNextMoves = [(p, getMoves state p) | p <- board, pColor (snd p) == turn]
    allStates = concat [statesForPiece state piece moves | (piece, moves) <- allNextMoves]
    search :: GameState -> Bool -> Bool
    search state@(turn,board,_) startingPlayer = let 
        foundWinner = isWinner state
        allMoves = allNextStates state
        in if not $ isNothing $ foundWinner
            then case foundWinner of
                Just x -> case x of 
                    Win y -> y == startingColor
                    Tie -> False
                Nothing -> error "this should never happen"                 
        else if startingPlayer
            then or $ map (\st -> search st (not startingPlayer)) allMoves
        else and $ map (\st -> search st (not startingPlayer)) allMoves
    evaluatedMoves = [(search state False, mv) | (mv, state) <- allStates]
    winningMoves = filter (\elem -> (fst elem) == True) evaluatedMoves
    in if null winningMoves then ((0,0), Piece Black King)
    else snd $ head winningMoves



bestOption :: GameState -> Move
bestOption curState@(turn, board,_) = let
    allMoves = [(p, getMoves curState p) | p <- board, pColor (snd p) == turn]
    nextStates = concat [statesForPiece curState piece moves | (piece, moves) <- allMoves]
    evalStates = map (\(mv, state) -> (eval state, (mv,state))) nextStates
    in case turn of
        Black -> fst $ snd $ minimum evalStates
        White -> fst $ snd $ maximum evalStates

-- a node is represented by a game state, hold onto the depth
-- store the "maximizer" as whoevers turn it is in origional state

allNextStates :: GameState -> [GameState] 
allNextStates state@(turn,bd,_) = let
    allMoves = [(p, getMoves state p) | p <- bd, pColor (snd p) == turn]
    getStates state from moves = [makeMove state from to | (to,p) <- moves]
    in concat [getStates state piece moves | (piece, moves) <- allMoves]

type Depth = Integer
type Maximizer = Bool


greedyPlay :: GameState -> Depth -> Move
greedyPlay state@(turn,bd,mvs) d = let
    allMoves = [(p, getMoves state p) | p <- bd, pColor (snd p) == turn]
    allStates = concat [statesForPiece state piece moves | (piece, moves) <- allMoves]
    in case turn of
        Black -> 
            snd $ minimum $ map (\(mv,st) -> (minimax st d False, mv)) allStates
        White -> 
            snd $ maximum $ map (\(mv,st) -> (minimax st d True, mv)) allStates

minimax :: GameState -> Depth -> Maximizer -> EvalScore
-- leaf nodes are nodes with a winner or depth == 0
-- if not isNothing isWinner state then terminal node
minimax state@(turn,bd,mvs) remDepth isMaximizer = 
    if remDepth == 0 || not (isNothing (isWinner state)) -- base case
        then eval state
    else if isMaximizer
        -- out of all the possible moves pick the max between maxVal and all minimax results
        then let allMoves = allNextStates state 
        in  maximum $ map (\state -> minimax state (remDepth-1) (not isMaximizer)) allMoves
    else -- not maximizer
        let allMoves = allNextStates state 
        in  minimum $ map (\state -> minimax state (remDepth-1) (not isMaximizer)) allMoves
         


-------------------------------------------
--               Test Code
-------------------------------------------

testGetMoves = getMoves (pawncolor, pawnTestState, pawnturns) ((5,8),Piece Black King)

myGameState = readState "2k4r/ppp4p/2b3p1/6r1/3pP2R/P2B4/3K2P1/8 w - - 0 28"

obviousMoveBlack = readState "rnbqkbnr/ppp1pp1p/6p1/3p3Q/3P4/4P3/PPP2PPP/RNB1KBNR b KQkq - 1 3"
obviousMoveWhite = readState "rnb1kbnr/ppp1pppp/8/3p4/3P2q1/4P2P/PPP2PP1/RNBQKBNR w KQkq - 1 4"

-- mates
mateInOneWhite = readState "1RR2rk1/5ppp/8/8/8/8/5PPP/6K1 w - - 0 3"
mateInTwoWhite = readState "1R3rk1/2R2ppp/8/8/8/1Q6/5PPP/6K1 w - - 0 5"
mateInThreeWhite = readState "1R2rrk1/2R2ppp/8/8/8/1Q6/5PPP/6K1 w - - 0 40"
mateInThreeKnightWhite = readState "3r4/7p/2R5/6k1/8/4N1PP/5PK1/8 w - - 0 40"

startingState = readState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 3"

startingBoard = getBoard startingState

startingStateBlack = readState"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1"

midgameState = readState "r1b1kb1r/p4p1p/1qp2np1/3p4/2pP4/2N1PN2/PP2QPPP/R1B1K2R w KQkq - 0 11"
midgameBoard = getBoard midgameState

sampleState = readState "r1b1kb1r/ppp1pppp/8/3pn3/3P4/4P1P1/P2N1PP1/3K1B1R w kq - 0 13"
blkFavBoard = getBoard sampleState

(blackColor, blackTurnBoard, blackTurns) = readState "r3kb1r/ppp3pp/5p2/3p1b2/6P1/4P3/P2NBPP1/3K3R b kq - 0 16"

(sample2color, sampleState2Board, sample2turns) = readState "r1b4r/p2k1pbp/1qp2np1/3P4/2pP4/2N2N2/PP2QPPP/R1B1K2R w KQ - 1 13"

(pawncolor, pawnTestState, pawnturns) = readState "r2qkb1r/1pp2p2/2npbn2/pP2p2p/3P2p1/2N1PN1P/P1P2PP1/R1BQKB1R w kq - 2 10"
pawnTestBoard = pawnTestState

(winnercolor, winnerBoard, winnerTurns) = readState "8/8/8/8/8/8/8/8 w kq - 2 10"


testWhiteWinning = readState "R2QK2R/8/8/8/8/8/8/3k4 w kq - 2 3"

testBlackWinning = readState "3K4/8/8/8/8/8/8/r2qk2r b kq - 2 4"

blackQueen = Piece Black Queen

testTie = readState "3K4/8/8/8/8/8/8/r2qk2r b kq - 2 0"


{-
testGetMoves :: [(RowNum, ColNum)]
testGetMoves = getMoves pawnTestState ((5,5),Piece Black Pawn)
testIncorrectGetMoves = getMoves startingState ((2,5),Piece White King)

-- testAllPieces :: GameState -> [(Piece, [(Char, RowNum)])]
-- testAllPieces state@(_,board, turns) = [(piece, prettyMoves (map fst (getMoves state (loc, piece))) | (loc, piece) <- board]
-}


