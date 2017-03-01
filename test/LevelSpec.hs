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
        let testMapSingle = ["#"]
        stringsToLevel testMapSingle `shouldBe` emptyLevel { _lTiles = M.fromList [((0,0), Wall)] }
        let testMapSmall =
              [ "##~"
              , "#.#"
              , "###"]
        stringsToLevel testMapSmall `shouldBe` emptyLevel
          { _lTiles
          = M.fromList
            [ ((0,0), Wall)
            , ((0,1), Wall)
            , ((0,2), Wall)
            , ((1,0), Wall)
            , ((1,1), Empty)
            , ((1,2), Wall)
            , ((2,0), Water)
            , ((2,1), Wall)
            , ((2,2), Wall)
            ]
          , _lMax = (2,2)
          }
