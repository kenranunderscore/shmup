{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Test.Hspec

import Shmup.Terminal.ParserSpec qualified as ParserSpec

spec = do
    ParserSpec.spec

main = hspec spec
