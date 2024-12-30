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

data Builtin
    = Exit
    | Cd FilePath
    | Pwd
    | Echo String
    deriving (Show, Eq)

data Command
    = Builtin Builtin
    | Other (NonEmpty String)
    | None
    deriving (Show, Eq)

readCommand :: String -> Command
readCommand line =
    case words line of
        [] -> None
        "exit" : _ -> Builtin Exit
        "cd" : dir : _ -> Builtin (Cd dir)
        "pwd" : _ -> Builtin Pwd
        "echo" : msg -> Builtin (Echo $ unwords msg)
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
            Builtin Exit -> do
                putStrLn "Exiting..."
            Builtin (Cd dir) -> do
                putStrLn $ "Changing directory to: " <> dir
                Dir.setCurrentDirectory dir
                go
            Builtin Pwd -> do
                pwd <- Dir.getCurrentDirectory
                putStrLn $ "Current directory: " <> pwd
                go
            Builtin (Echo msg) -> putStrLn msg *> go
            Other (cmd NonEmpty.:| args) -> do
                putStrLn $ "The command was: " <> cmd <> ", " <> show args
                Proc.callProcess cmd args
                go
