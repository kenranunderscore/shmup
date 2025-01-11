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
import Foreign.Marshal.Safe (with)
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

normalText :: Parser String
normalText = many1 (noneOf "\x1b")

isBetween :: Int -> Int -> Char -> Bool
isBetween l h c = let o = ord c in o >= l && o <= h

isIntermediate :: Char -> Bool
isIntermediate = isBetween 0x20 0x2f

intermediate :: Parser Char
intermediate = satisfy isIntermediate

isParameter :: Char -> Bool
isParameter = isBetween 0x30 0x3f

isAlphabetic :: Char -> Bool
isAlphabetic = isBetween 0x40 0x7e

parameter :: Parser Char
parameter = satisfy isParameter

uppercase :: Parser Char
uppercase = satisfy (isBetween 0x40 0x5f)

lowercase :: Parser Char
lowercase = satisfy (isBetween 0x60 0x7e)

alphabetic :: Parser Char
alphabetic = satisfy isAlphabetic

delete :: Parser Char
delete = char '\DEL'

esc :: Parser ()
esc = void $ char '\ESC'

controlSeq :: Parser EscapeSeq
controlSeq = do
    char '['
    params <- many (digit <|> oneOf ":;<=>?")
    void $ skipMany intermediate
    terminator <- letter
    pure $ ControlSeq $ params <> [terminator]

-- TODO: C0, C1, G1
data EscapeSeq
    = Private String
    | StandardSeq String
    | ControlSeq String
    | Uppercase Char -- TODO: convert to C1
    deriving stock (Show, Eq)

intermediateSeq :: Parser EscapeSeq
intermediateSeq = do
    int <- many1 intermediate
    c <- anyChar
    let res = int <> [c]
    if
        | isParameter c -> pure $ Private res
        | isAlphabetic c -> pure $ StandardSeq res
        | otherwise -> unexpected "expected param or alpha after intermediate"

privateSeq :: Parser EscapeSeq
privateSeq = Private . (\c -> [c]) <$> parameter

escapeSeq :: Parser EscapeSeq
escapeSeq = do
    esc
    optional delete
    choice
        [ intermediateSeq
        , privateSeq
        , controlSeq
        , Uppercase <$> uppercase
        , StandardSeq . (\c -> [c]) <$> lowercase
        ]

data Rune
    = Glyph Char
    | EscapeSequence EscapeSeq
    deriving stock (Show, Eq)

type TerminalOutput = [Rune]

rune :: Parser Rune
rune =
    (EscapeSequence <$> controlSeq)
        <|> (EscapeSequence <$> escapeSeq)
        <|> (Glyph <$> noneOf "\x1b")

terminalOutput :: Parser TerminalOutput
terminalOutput = many rune

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
        drawing $ do
            clearBackground black
            drawFPS 1800 0
            with font $ \font' -> do
                with green $ \green' -> do
                    foldM_
                        ( \(row, col) -> \case
                            Glyph c -> do
                                case c of
                                    '\r' -> pure (row, col)
                                    '\n' -> pure (row + 1, 0)
                                    _ -> do
                                        let x = realToFrac col * w
                                        let y = realToFrac row * h
                                        drawTextEx' font' [c] x y fontSize 0 green'
                                        pure (row, col + 1)
                            _ -> pure (row, col)
                        )
                        (0 :: Integer, 0 :: Integer)
                        termOutput
        mainLoop window font dimensions content fd

foreign import ccall "ioctl" ioctl :: Posix.Fd -> CUInt -> Ptr () -> IO CInt
foreign import ccall "setsid" setsid :: IO ()
foreign import ccall unsafe "draw_text_ex_no_vec" drawTextExNoVec :: Ptr Font -> CString -> Float -> Float -> Float -> Float -> Ptr Color -> IO ()

{-# INLINE drawTextEx' #-}
drawTextEx' :: Ptr Font -> String -> Float -> Float -> Float -> Float -> Ptr Color -> IO ()
drawTextEx' font text x y size spacing color =
    withCString text $ \s ->
        drawTextExNoVec font s x y size spacing color

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
    withWindow 2000 1500 "the best is yet to shmup" 2000 $ \window -> do
        font <-
            managed window $
                loadFontEx
                    "/home/void/.nix-profile/share/fonts/truetype/JetBrainsMono-Regular.ttf"
                    (round fontSize)
                    mempty
        dimensions <- measureTextEx font "m" fontSize 0
        mainLoop window font dimensions content primary
