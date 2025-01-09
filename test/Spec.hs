{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Text.Parsec

import Shmup.Main hiding (main)

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

main :: IO ()
main = hspec spec
