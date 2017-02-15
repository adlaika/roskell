{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module LibSpec (spec) where

import Data.Map as M

import Test.Hspec

import Lib

spec :: Spec
spec = do
  describe "Level" $ do
    context "generateLevel" $ do
      it "generates a size 0 level" $ do
        let expected = Level
              { _bCells = M.empty
              , _bSize = 0
              }
        let actual = generateLevel 0
        expected `shouldBe` actual
