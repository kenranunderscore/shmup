{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Test.Hspec
import Text.Parsec

import Shmup.Terminal

spec :: Spec
spec = do
    describe "CSI" $ do
        it "can be parsed" $ do
            parse escapeSeq "escape" "\ESC[2m" `shouldBe` Right (CSI "2m")
    describe "no starter" $ do
        it "can be parsed" $ do
            parse escapeSeq "escape" "\ESC17;13m" `shouldBe` Right (NoStarter "17;13m")
    describe "DCS" $ do
        it "can be parsed" $ do
            parse escapeSeq "escape" "\ESCP2m" `shouldBe` Right (DCS "2m")
    describe "OSC" $ do
        it "can be parsed" $ do
            parse escapeSeq "escape" "\ESC]7m" `shouldBe` Right (OSC "7m")
    describe "terminal output" $ do
        it "can be parsed" $ do
            let Right res = parse terminalOutput "" "A \ESC[31mb c\ESC[0m"
            res
                `shouldBe` [ Left 'A'
                           , Left ' '
                           , Right (CSI "31m")
                           , Left 'b'
                           , Left ' '
                           , Left 'c'
                           , Right (CSI "0m")
                           ]

main :: IO ()
main = hspec spec
