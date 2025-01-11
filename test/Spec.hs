{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Test.Hspec
import Text.Parsec

import Shmup.Terminal

spec = do
    termOutputSpec
    escapeSeqSpec

escapeSeqSpec = do
    describe "intermediate cases" $ do
        it "standard seq" $ do
            parse escapeSeq "esc" "\ESC(A" `shouldBe` Right (StandardSeq "(A")
        it "private seq" $ do
            parse escapeSeq "esc" "\ESC(0" `shouldBe` Right (Private "(0")
        it "ignores del" $ do
            parse escapeSeq "esc" "\ESC\DEL(A" `shouldBe` Right (StandardSeq "(A")
    describe "parameter case" $ do
        it "private seq" $ do
            parse escapeSeq "esc" "\ESC=" `shouldBe` Right (Private "=")
    describe "uppercase case" $ do
        it "D" $ do
            parse escapeSeq "esc" "\ESCD" `shouldBe` Right (Uppercase 'D')
    describe "lowercase case" $ do
        it "standard seq" $ do
            parse escapeSeq "esc" "\ESCc" `shouldBe` Right (StandardSeq "c")
    describe "control seq" $ do
        it "can be parsed" $ do
            parse escapeSeq "control" "\ESC[2m" `shouldBe` Right (ControlSeq "2m")

termOutputSpec = do
    describe "colored output" $ do
        it "can be parsed" $ do
            let Right res = parse terminalOutput "" "A \ESC[31mb c\ESC[0m"
            res
                `shouldBe` [ Glyph 'A'
                           , Glyph ' '
                           , EscapeSequence (ControlSeq "31m")
                           , Glyph 'b'
                           , Glyph ' '
                           , Glyph 'c'
                           , EscapeSequence (ControlSeq "0m")
                           ]
    describe "fish prompt" $ do
        it "can be parsed" $ do
            let res =
                    parse
                        terminalOutput
                        ""
                        "\ESC[92mvoid\ESC(B\ESC[m@\ESC(B\ESC[mtuon\ESC(B\ESC[m \ESC[32m~/p/s/main\ESC(B\ESC[m (main)\ESC(B\ESC[m> \ESC[K\r\ESC[29C"
            res
                `shouldBe` Right
                    [ EscapeSequence (ControlSeq "92m")
                    , Glyph 'v'
                    , Glyph 'o'
                    , Glyph 'i'
                    , Glyph 'd'
                    , EscapeSequence (StandardSeq "(B")
                    , EscapeSequence (ControlSeq "m")
                    , Glyph '@'
                    , EscapeSequence (StandardSeq "(B")
                    , EscapeSequence (ControlSeq "m")
                    , Glyph 't'
                    , Glyph 'u'
                    , Glyph 'o'
                    , Glyph 'n'
                    , EscapeSequence (StandardSeq "(B")
                    , EscapeSequence (ControlSeq "m")
                    , Glyph ' '
                    , EscapeSequence (ControlSeq "32m")
                    , Glyph '~'
                    , Glyph '/'
                    , Glyph 'p'
                    , Glyph '/'
                    , Glyph 's'
                    , Glyph '/'
                    , Glyph 'm'
                    , Glyph 'a'
                    , Glyph 'i'
                    , Glyph 'n'
                    , EscapeSequence (StandardSeq "(B")
                    , EscapeSequence (ControlSeq "m")
                    , Glyph ' '
                    , Glyph '('
                    , Glyph 'm'
                    , Glyph 'a'
                    , Glyph 'i'
                    , Glyph 'n'
                    , Glyph ')'
                    , EscapeSequence (StandardSeq "(B")
                    , EscapeSequence (ControlSeq "m")
                    , Glyph '>'
                    , Glyph ' '
                    , EscapeSequence (ControlSeq "K")
                    , Glyph '\r'
                    , EscapeSequence (ControlSeq "29C")
                    ]

main = hspec spec
