{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Shmup.Terminal where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (foldM_, forever, void, when)
import Data.ByteString.Char8 qualified as BS
import Data.Char (chr, ord, toLower)
import Debug.Trace
import Foreign (Ptr)
import Foreign.C
import Foreign.Ptr (nullPtr)
import Raylib.Core
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import System.Posix qualified as Posix
import System.Posix.ByteString qualified as PosixBS
import Text.Parsec
import Text.Parsec.ByteString

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb action = mb >>= (`when` action)

readPty :: Posix.Fd -> IO (TVar BS.ByteString)
readPty fd = do
    res <- newTVarIO mempty
    _ <- forkIO $ forever $ do
        output <- PosixBS.fdRead fd 4096
        traceM $ "  OUTPUT: " <> show output
        atomically $ modifyTVar' res (<> output)
    pure res

readKeys :: IO String
readKeys = reverse <$> go []
  where
    go acc = do
        key <- getKeyPressed
        when (key /= KeyNull) (traceM $ "   key: " <> show key)
        let i = fromEnum key
        let c = toLower (chr i)
        if
            | key == KeySpace -> go (c : acc)
            | key == KeyEnter -> go ('\r' : acc)
            | i > 96 -> go acc
            | i < 39 -> pure acc
            | otherwise -> go (c : acc)

fontSize :: Float
fontSize = 50

data ControlSequence
    = NoStarter String
    | CSI String
    | DCS String
    | OSC String
    deriving (Show, Eq)

normalText :: Parser String
normalText = many1 (noneOf "\x1b")

isIntermediate :: Char -> Bool
isIntermediate c = ord c >= 0x20 && ord c <= 0x2f

escapeSeq :: Parser ControlSequence
escapeSeq = do
    char '\ESC'
    starter <- optionMaybe (oneOf "[]P")
    params <- many (digit <|> oneOf ":;<=>?")
    void $ skipMany (satisfy isIntermediate)
    terminator <- letter
    let ctor = case starter of
            Nothing -> NoStarter
            Just '[' -> CSI
            Just ']' -> OSC
            Just 'P' -> DCS
            _ -> error "impossibile"
    pure $ ctor (params <> [terminator])

type TerminalOutput = [Either Char ControlSequence]

terminalOutput :: Parser TerminalOutput
terminalOutput = do
    many $ (Right <$> escapeSeq) <|> (Left <$> noneOf "\x1b")

parseOutput :: BS.ByteString -> TerminalOutput
parseOutput output =
    case parse terminalOutput "terminal output" output of
        Left err -> error (show err)
        Right res -> res

mainLoop ::
    WindowResources ->
    Font ->
    Vector2 ->
    TVar BS.ByteString ->
    Posix.Fd ->
    IO ()
mainLoop window font dimensions@(Vector2 w h) content fd = do
    whenM (not <$> windowShouldClose) $ do
        input <- readKeys
        Posix.fdWrite fd input
        output <- readTVarIO content
        let termOutput = parseOutput output
        -- traceM $ "   term output: " <> show termOutput
        drawing $ do
            clearBackground black
            foldM_
                ( \(row, col) -> \case
                    Left c -> do
                        case c of
                            '\r' -> pure (row, col)
                            '\n' -> pure (row + 1, 0)
                            _ -> do
                                let x = realToFrac col * w
                                let y = realToFrac row * h
                                drawTextEx font [c] (Vector2 x y) fontSize 0 green
                                pure (row, col + 1)
                    Right _seq -> pure (row, col)
                )
                (0 :: Integer, 0 :: Integer)
                termOutput
        mainLoop window font dimensions content fd

foreign import ccall "ioctl" ioctl :: Posix.Fd -> CUInt -> Ptr () -> IO CInt
foreign import ccall "setsid" setsid :: IO ()

tiocsctty :: CUInt
tiocsctty = 21518

run :: IO ()
run = do
    (primary, secondary) <- Posix.openPseudoTerminal
    Posix.forkProcess $ do
        Posix.closeFd primary
        -- TODO: understand
        -- TODO: error handling
        setsid
        void $ ioctl secondary tiocsctty nullPtr
        void $ Posix.dupTo secondary Posix.stdInput
        void $ Posix.dupTo secondary Posix.stdError
        void $ Posix.dupTo secondary Posix.stdOutput
        Posix.closeFd secondary
        Posix.executeFile "bash" True [] Nothing
    Posix.closeFd secondary
    content <- readPty primary
    withWindow 2000 1500 "the best is yet to shmup" 30 $ \window -> do
        font <-
            managed window $
                loadFontEx
                    "/home/void/.nix-profile/share/fonts/truetype/JetBrainsMono-Regular.ttf"
                    (round fontSize)
                    mempty
        dimensions <- measureTextEx font "m" fontSize 0
        mainLoop window font dimensions content primary
