module Shmup.Main (main) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import System.Directory qualified as Dir
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)
import System.Process qualified as Proc

drawPrompt :: IO ()
drawPrompt = do
    putStr "$ "
    hFlush stdout

data Command
    = None
    | Exit
    | CD FilePath
    | Echo String
    | PWD
    | Other (NonEmpty String)
    deriving stock (Show, Eq)

readCommand :: String -> Command
readCommand line =
    case words line of
        [] -> None
        "exit" : _ -> Exit
        "cd" : dir : _ -> (CD dir)
        "pwd" : _ -> PWD
        "echo" : msg -> (Echo $ unwords msg)
        (x : xs) -> Other (x NonEmpty.:| xs)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    go
  where
    go = do
        drawPrompt
        line <- getLine
        case readCommand line of
            None -> go
            Exit -> do
                putStrLn "Exiting..."
            CD dir -> do
                putStrLn $ "Changing directory to: " <> dir
                Dir.setCurrentDirectory dir
                go
            PWD -> do
                pwd <- Dir.getCurrentDirectory
                putStrLn $ "Current directory: " <> pwd
                go
            Echo msg -> putStrLn msg *> go
            Other (cmd NonEmpty.:| args) -> do
                putStrLn $ "The command was: " <> cmd <> ", " <> show args
                Proc.callProcess cmd args
                go
