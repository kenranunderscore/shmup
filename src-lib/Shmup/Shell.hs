{-# LANGUAGE OverloadedRecordDot #-}

module Shmup.Shell where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)
import System.Posix qualified as Posix
import System.Process qualified as Proc

data Command
    = None
    | Exit
    | CD FilePath
    | Echo String
    | PWD
    | ClearScreen
    | Other (NonEmpty String)
    deriving (Show, Eq)

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
    , pwd :: FilePath
    , lastError :: Maybe Int
    }
    deriving stock (Show)

initialState :: FilePath -> S
initialState pwd =
    S
        { input = mempty
        , pwd
        , lastError = Nothing
        }

class Monad m => TTY m where
    flush :: m ()
    getch :: m Char
    putch :: Char -> m ()
    write :: String -> m ()

instance TTY IO where
    flush = liftIO $ hFlush stdout
    getch = liftIO getChar
    putch = liftIO . putChar
    write = liftIO . putStr

instance TTY (StateT s IO) where
    flush = lift flush
    getch = lift getch
    putch = lift . putch
    write = lift . write

writeLn :: TTY m => String -> m ()
writeLn s = do
    write s
    putch '\n'
    flush

drawPrompt :: Shell m => m ()
drawPrompt = do
    lastError <- State.gets (.lastError)
    let prompt = case lastError of
            Nothing -> "$ "
            Just code -> "[" <> show code <> "] $ "
    write prompt
    flush

readInput :: Shell m => m Command
readInput = do
    c <- getch
    case c of
        '\f' -> do
            pure ClearScreen
        '\n' -> do
            input <- State.gets (.input)
            State.modify' (\s -> s{input = mempty})
            pure $ parseCommand input
        _ -> do
            State.modify' (\s -> s{input = s.input ++ [c]})
            readInput

type Shell m = (TTY m, MonadState S m)

runChildProcess :: (Shell m, MonadIO m) => String -> [String] -> m ()
runChildProcess cmd args = do
    pwd <- State.gets (.pwd)
    let createProcess = (Proc.proc cmd args){Proc.cwd = Just pwd}
    (_, _, _, handle) <- liftIO $ Proc.createProcess createProcess
    exitCode <- liftIO $ Proc.waitForProcess handle
    case exitCode of
        ExitSuccess -> State.modify' (\s -> s{lastError = Nothing})
        ExitFailure code -> State.modify' (\s -> s{lastError = Just code})

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
            State.modify' (\s -> s{pwd = dir})
            run
        PWD -> do
            pwd <- State.gets (.pwd)
            writeLn $ "Current directory: " <> pwd
            run
        Echo msg -> do
            writeLn msg
            run
        ClearScreen -> do
            write "\ESC[2J\ESC[H"
            flush
            run
        Other (cmd NonEmpty.:| args) -> do
            writeLn $ "The command was: " <> cmd <> ", " <> show args
            runChildProcess cmd args
            run

enableRawMode :: IO Posix.TerminalAttributes
enableRawMode = do
    oldAttrs <- Posix.getTerminalAttributes Posix.stdInput
    let newAttrs =
            oldAttrs
                & withoutMode Posix.EnableEcho
                & withoutMode Posix.ProcessInput
                & withoutMode Posix.KeyboardInterrupts
    Posix.setTerminalAttributes Posix.stdInput newAttrs Posix.Immediately
    pure oldAttrs
  where
    withoutMode = flip Posix.withoutMode

shell :: IO ()
shell = do
    hSetBuffering stdout LineBuffering
    termAttrs <- enableRawMode
    pwd <- Dir.getCurrentDirectory
    void $ State.runStateT run (initialState pwd)
    Posix.setTerminalAttributes Posix.stdInput termAttrs Posix.Immediately
