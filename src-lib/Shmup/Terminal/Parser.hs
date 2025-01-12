{-# LANGUAGE MultiWayIf #-}

module Shmup.Terminal.Parser where

import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Text.Parsec
import Text.Parsec.ByteString

normalText :: Parser String
normalText = many1 (noneOf "\x1b")

isBetween :: Int -> Int -> Char -> Bool
isBetween l h c = let o = Char.ord c in o >= l && o <= h

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
    void $ char '['
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
