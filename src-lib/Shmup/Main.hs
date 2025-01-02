{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Shmup.Main (main) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import System.Directory qualified as Dir
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)
import System.Process qualified as Proc

data Command
    = None
    | Exit
    | CD FilePath
    | Echo String
    | PWD
    | Other (NonEmpty String)
    deriving stock (Show, Eq)

parseCommand :: String -> Command
parseCommand line =
    case words line of
        [] -> None
        "exit" : _ -> Exit
        "cd" : dir : _ -> (CD dir)
        "pwd" : _ -> PWD
        "echo" : msg -> (Echo $ unwords msg)
        (x : xs) -> Other (x NonEmpty.:| xs)

data S = S
    { input :: String
    }
    deriving stock (Show)

class (Monad m) => TTY m where
    flush :: m ()
    getch :: m Char
    putch :: Char -> m ()
    write :: String -> m ()

instance (Monad m, MonadIO m) => TTY m where
    flush = liftIO $ hFlush stdout
    getch = liftIO getChar
    putch = liftIO . putChar
    write = liftIO . putStr

writeLn :: (TTY m) => String -> m ()
writeLn s = do
    write s
    putch '\n'
    flush

drawPrompt :: (TTY m) => m ()
drawPrompt = write "$ " >> flush

data Input
    = Unfinished String
    | Command Command
    deriving stock (Show)

readInput :: (Shell m) => m Command
readInput = do
    c <- getch
    case c of
        '\n' -> do
            input <- State.gets (.input)
            State.modify' (\s -> s{input = mempty})
            pure $ parseCommand input
        _ -> do
            State.modify' (\s -> s{input = s.input ++ [c]})
            readInput

type Shell m = (TTY m, MonadState S m)

run :: (Shell m, MonadIO m) => m ()
run = do
    drawPrompt
    command <- readInput
    case command of
        None -> run
        Exit -> do
            writeLn "Exiting..."
        CD dir -> do
            writeLn $ "Changing directory to: " <> dir
            liftIO $ Dir.setCurrentDirectory dir
            run
        PWD -> do
            pwd <- liftIO Dir.getCurrentDirectory
            writeLn $ "Current directory: " <> pwd
            run
        Echo msg -> do
            writeLn msg
            run
        Other (cmd NonEmpty.:| args) -> do
            writeLn $ "The command was: " <> cmd <> ", " <> show args
            liftIO $ Proc.callProcess cmd args
            run

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    void $ State.runStateT run (S mempty)
