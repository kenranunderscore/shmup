module Shmup.Main (main) where

import System.IO (BufferMode (..), hFlush, hSetBuffering, stdout)

drawPrompt :: IO ()
drawPrompt = do
    putStr "$ "
    hFlush stdout

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    go
  where
    go = do
        drawPrompt
        cmd <- getLine
        case cmd of
            "exit" -> pure ()
            _ -> do
                go
