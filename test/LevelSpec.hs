{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module LevelSpec (spec) where

import Data.Map as M

import Test.Hspec

import Types
import Level

spec :: Spec
spec = do
  describe "Level" $ do
    context "stringsToLevel" $ do
      it "should correctly translate a set of strings to a level" $ do
        let emptyMap = [""]
        stringsToLevel emptyMap `shouldBe` emptyLevel { _lTiles = M.empty }
        let testMap = ["#"]
        stringsToLevel testMap `shouldBe` emptyLevel { _lTiles = M.fromList [((0,0), Wall)] }
