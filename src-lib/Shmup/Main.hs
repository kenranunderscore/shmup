{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Shmup.Main (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)
import System.Posix qualified as Posix
import System.Posix.ByteString qualified as PosixBS
import System.Process qualified as Proc

import Data.Char (chr)
import Foreign (Ptr)
import Foreign.C
import Foreign.Ptr (nullPtr)
import Raylib.Core
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors

data Command
    = None
    | Exit
    | CD FilePath
    | Echo String
    | PWD
    | ClearScreen
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

instance (Monad m, MonadIO m) => TTY m where
    flush = liftIO $ hFlush stdout
    getch = liftIO getChar
    putch = liftIO . putChar
    write = liftIO . putStr

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

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= (`when` action)

readPty :: Posix.Fd -> IO (TVar String)
readPty fd = do
    res <- newTVarIO mempty
    _ <- forkIO $ forever $ do
        output <- PosixBS.fdRead fd 4096
        let text = map (chr . fromEnum) (BS.unpack output)
        atomically $ swapTVar res text
    pure res

mainLoop :: WindowResources -> TVar String -> IO ()
mainLoop window content = do
    font <-
        managed window $
            loadFontEx
                "/home/void/.nix-profile/share/fonts/truetype/CourierPrime-Regular.ttf"
                (round fontSize)
                mempty
    go font
  where
    fontSize = 50
    go font = do
        whenM (not <$> windowShouldClose) $ do
            putStrLn "   ...reading chan"
            text <- readTVarIO content
            putStrLn "   ...read chan"
            drawing $ do
                clearBackground black
                drawTextEx font text (Vector2 20 12) fontSize 0 green
            go font

foreign import ccall "ioctl" ioctl :: Posix.Fd -> CUInt -> Ptr () -> IO CInt
foreign import ccall "setsid" setsid :: IO ()

tiocsctty :: CUInt
tiocsctty = 21518

main :: IO ()
main = do
    (primary, secondary) <- Posix.openPseudoTerminal
    Posix.forkProcess $ do
        Posix.closeFd primary
        setsid
        -- TODO: error handling
        void $ ioctl secondary tiocsctty nullPtr
        void $ Posix.dupTo secondary Posix.stdInput
        void $ Posix.dupTo secondary Posix.stdError
        void $ Posix.dupTo secondary Posix.stdOutput
        Posix.closeFd secondary
        Posix.executeFile "/bin/dash" True [] Nothing
    Posix.closeFd secondary
    content <- readPty primary
    withWindow 2000 1500 "the best is yet to shmup" 60 $ \window -> do
        mainLoop window content
        closeWindow (Just window)
