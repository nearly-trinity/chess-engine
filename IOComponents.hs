module IOComponents where
import Engine
import Data.List
import System.IO
import System.Environment
import Data.Maybe
--------------------------------------------
--               IO Stuff
--------------------------------------------               

loadGame :: FilePath -> IO GameState
loadGame f =
    do gs <- readFile f
       return (readState gs)

writeGame :: GameState -> FilePath -> IO ()
writeGame gs f =
    do writeFile f (toFEN gs)

toFEN :: GameState -> String
toFEN (c, b, t) =
    let rowsFEN = map (rowToFEN b) [8,7..1]
    in concat $ rowsFEN ++ [" w "] ++ [show t]

rowToFEN :: Board -> Int -> String
rowToFEN b r =
    let rowStr = map (makePiece b) ([(i, r) | i <- [1 .. 8]])
        groups = groupBy (\x y -> not $ (x == " " || y == " ") && (x /= y)) rowStr
        fen = map (\lst -> if ((head lst) == " ") then show $ length lst else concat lst) groups
    in concat $ if (r /= 1) then fen ++ ["/"] else fen

putWinner :: GameState -> IO ()
putWinner gs@(color,board, turns) =
    case isWinner gs of
        Nothing -> return ()
        Just x -> putStrLn $ "The outcome is: \"" ++ show x ++ "\""

putWinner2 :: GameState -> Bool -> IO ()
putWinner2 gs@(color,board,turns) isVerbose = 
        case whoWillWin gs of
                Tie -> putStrLn $ "Tie"
                Win x -> if isVerbose then putStrLn $ "The outcome is: " ++ show x ++ " wins!" else putStrLn $ show x
 
----------------------------------------------------------------------------
--                          Display Board
---------------------------------------------------------------------------- 

-- Used to offset lines when printing the board
offsetStr = "  "

format :: [String] -> String
format strs = concat $ strs ++ ["\n"]

-- Returns the strings with an offset at the front
offset :: [String] -> [String]
offset strs = offsetStr : strs

-- Returns a nicely formatted string that represents a board
printBoard :: GameState -> IO ()
printBoard (_,board,_) = putStr $ displayBoard board

displayBoard :: Board -> String
displayBoard b = makeRows b ++ makeLine ++ makeBorder

-- Creates the line with the labels for the columns of the board
makeBorder :: String
makeBorder = format $ offset (map (\x -> "  " ++ [x] ++ " ") ['a'..'h'])

-- Creates a string for all of the rows of the board
makeRows:: Board -> String
makeRows b = concatMap (makeRow b) [8,7..1]

-- Creates a string for a given row of the board
makeRow :: Board -> Int -> String
makeRow b n = format [makeLine, show n, " ", makeContent b n]

-- Creates a horizontal line
makeLine :: String
makeLine =  format $ offset (replicate 8 " ---")

-- Makes the middle of the cells
makeContent :: Board -> Int -> String
makeContent b n = concat $ ([(\ x -> "| " ++ makePiece b x ++ " ") (i, n) | i <- [1 .. 8]]) ++ ["|"]

-- Returns the representation of a piece, or a space if there is no piece at that location
makePiece :: Board -> Location -> String
makePiece b loc =
    maybe " " show (lookup loc b)


