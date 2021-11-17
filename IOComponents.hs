module IOComponents where 
import Engine
import DisplayBoard
import Data.List
import System.IO
import System.Environment
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
toFEN (c, b) = 
    let rowsFEN = map (rowToFEN b) [8,7..1]
    in concat $ rowsFEN ++ [" w "]

rowToFEN :: Board -> Int -> String
rowToFEN b r = 
    let rowStr = map (makePiece b) ([(i, r) | i <- [1 .. 8]])
        groups = groupBy (\x y -> not $ (x == " " || y == " ") && (x /= y)) rowStr
        fen = map (\lst -> if((head lst) == " ") then show $ length lst else concat lst) groups
    in concat $ if(r /= 1) then fen ++ ["/"] else fen 

putWinner :: GameState -> IO ()
putWinner gs = undefined

