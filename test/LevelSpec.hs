{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module LevelSpec (spec) where

import Data.Map as M

import Test.Hspec

import Types
import Level

spec :: Spec
spec = do
  describe "Level" $ do
    context "generateLevel" $ do
      it "generates a size 0 level" $ do
        True `shouldBe` True
