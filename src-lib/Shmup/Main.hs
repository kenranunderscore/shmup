module Shmup.Main (main) where

import System.Directory qualified as Dir
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)

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
    | Other String
    deriving (Show, Eq)

readCommand :: String -> Command
readCommand line =
    case words line of
        [] -> error "impossible?"
        "exit" : _ -> Builtin Exit
        "cd" : dir : _ -> Builtin (Cd dir)
        "pwd" : _ -> Builtin Pwd
        "echo" : msg : _ -> Builtin (Echo msg)
        _ -> Other line

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    go
  where
    go = do
        drawPrompt
        line <- getLine
        case readCommand line of
            Builtin Exit -> do
                putStrLn "Exiting..."
                pure ()
            Builtin (Cd dir) -> do
                putStrLn $ "Changing directory to: " <> dir
                Dir.setCurrentDirectory dir
                go
            Builtin Pwd -> do
                pwd <- Dir.getCurrentDirectory
                putStrLn $ "Current directory: " <> pwd
                go
            Builtin (Echo msg) -> putStrLn msg *> go
            Other cmd -> do
                putStrLn $ "The command was: " <> cmd
                go
