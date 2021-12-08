module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Random
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Engine
import IOComponents
--import gamestates

data Flag = Help | Depth String | Winner | Verbose deriving (Eq, Show)

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit.", Option ['d'] ["depth"] (ReqArg Depth "<num>") "Use <num> Turns.", Option ['w'] ["winner"] (NoArg Winner) "Print winner with a non-exhaustive search.", Option ['v'] ["verbose"] (NoArg Verbose) "Output both the move and a description of how good it is: win, lose, tie, or a rating."]          

main :: IO ()
main =
    do args <- getArgs
       let (flags, inputs, errors) = getOpt Permute options args
       putStrLn $ show flags
       putStrLn $ show inputs
       if Help `elem` flags 
       then putStrLn $ usageInfo "Usage: chess [options] [file]" options
       else do 
         let fname = if null inputs then "gamestates.txt" else head inputs
         state <- loadGame fname
         let action | Winner `elem` flags && Verbose `elem` flags = output state True (getDepth flags)
                    | Winner `elem` flags = putStrLn $ "The best move for this state is: " ++ show (bestMove state)
                    | Verbose `elem` flags = output state True (getDepth flags)
                    | otherwise = putStrLn $ "The best move for maxDepth is: " ++ show (greedyPlay state (getDepth flags))
         action 
                       
output :: GameState -> Bool -> Integer -> IO()
output gs True depth =  
        do putStrLn $ "The best move for a cut-off depth of " ++ show depth ++ " is: " ++ show (greedyPlay gs depth)
           putStr $ "The game ended with this result: " 
           putWinner2 gs True
output gs False depth = 
        do putStrLn $ "The best move for a cut-off depth of " ++ show depth ++ " is: " ++ show (greedyPlay gs depth)


getDepth :: [Flag] -> Integer 
getDepth [] = maxDepth
getDepth (Depth s:flags) = read s
getDepth (_:flags) = if (getDepth flags > maxDepth) then maxDepth else getDepth flags
                        

