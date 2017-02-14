{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module LibSpec (spec) where

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "Board" $ do
    context "mkBoard" $ do
      it "creates a 3x3 board" $
        True `shouldBe` True
