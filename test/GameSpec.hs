module GameSpec where

import Test.Hspec

import Types
import Game
import Level

spec :: Spec
spec =
  describe "updateWorld" $ do
    it "given a movement input and a world, should move the hero" $ do
      let genesis = emptyWorld { _wHero = commoner { _hPos = (0,0) } }
      let walkRight = Walk DirRight
      let expectedAfterWalkRight = Right $ emptyWorld { _wHero = commoner { _hPos = (1,0) } }
      let actualAfterWalkRight = updateWorld walkRight genesis
      actualAfterWalkRight `shouldBe` expectedAfterWalkRight
      let walkLeft = Walk DirLeft
      let expectedAfterWalkLeft = Right $ emptyWorld { _wHero = commoner { _hPos = (-1,0) } }
      let actualAfterWalkLeft = updateWorld walkLeft genesis
      actualAfterWalkLeft `shouldBe` expectedAfterWalkLeft
      let walkUp = Walk DirUp
      let expectedAfterWalkUp = Right $ emptyWorld { _wHero = commoner { _hPos = (0,-1) } }
      let actualAfterWalkUp = updateWorld walkUp genesis
      actualAfterWalkUp `shouldBe` expectedAfterWalkUp
      let walkDown = Walk DirDown
      let expectedAfterWalkDown = Right $ emptyWorld { _wHero = commoner { _hPos = (0,1) } }
      let actualAfterWalkDown = updateWorld walkDown genesis
      actualAfterWalkDown `shouldBe` expectedAfterWalkDown
    it "the hero should be stopped by walls" $ do
      let ascii = [".#"]
      let genesis = emptyWorld
            { _wHero = commoner { _hPos = (0,0) }
            , _wLevel = stringsToLevel ascii }
      let walkRight = Walk DirRight
      let expectedAfterWalkRight = Left $ Collision Wall
      let actualAfterWalkRight = updateWorld walkRight genesis
      actualAfterWalkRight `shouldBe` expectedAfterWalkRight
