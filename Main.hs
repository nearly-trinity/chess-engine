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
import DisplayBoard
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
             states <- fmap lines $ readFile fname
             putStrLn $ "blahblah"
                       -- case getDepth flags of 
                       

getDepth :: [Flag] -> Int 
getDepth [] = 4
getDepth (Depth s:flags) = read s
getDepth (_:flags) = getDepth flags
                        

