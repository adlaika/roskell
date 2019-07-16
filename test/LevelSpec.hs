module LevelSpec where

import Data.Map as M

import Test.Hspec

import Types
import Level

sampleAscii :: [String]
sampleAscii =
      [ ".#~."
      , "#..~"
      , "#~.#"]

tile :: Int -> Int -> Tile -> ((Int, Int), Tile)
tile x y t = ((x,y), t)

sampleLevel :: Level
sampleLevel = emptyLevel
      { _lTiles = M.fromList
        [ tile 0 0 Empty, tile 1 0 Wall , tile 2 0 Water, tile 3 0 Empty
        , tile 0 1 Wall , tile 1 1 Empty, tile 2 1 Empty, tile 3 1 Water
        , tile 0 2 Wall , tile 1 2 Water, tile 2 2 Empty, tile 3 2 Wall
        ]
      , _lMax = (3,2)
      }

spec :: Spec
spec = do
  describe "stringsToLevel" $
    it "should correctly translate a set of strings to a level" $ do
      let emptyMap = [""]
      stringsToLevel emptyMap `shouldBe` emptyLevel { _lTiles = M.empty }
      let singleTileMap = ["#"]
      stringsToLevel singleTileMap `shouldBe` emptyLevel { _lTiles = M.fromList [tile 0 0 Wall] }
      stringsToLevel sampleAscii `shouldBe` sampleLevel

  describe "levelToStrings" $
    it "should correctly translate a level to a set of strings " $
      levelToStrings sampleLevel `shouldBe` sampleAscii
