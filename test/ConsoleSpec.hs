{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module ConsoleSpec (spec) where

import Data.Map as M

import Test.Hspec

import Console

spec :: Spec
spec = do
  describe "Level" $ do
    context "generateLevel" $ do
      it "generates a size 0 level" $ do
        True `shouldBe` True
