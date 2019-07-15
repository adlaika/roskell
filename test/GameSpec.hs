module GameSpec where

import Prelude hiding (Either(..))
import Test.Hspec

import Types
import Game

spec :: Spec
spec =
  describe "updateWorld" $
    it "given a movement input and a world, should move the hero" $ do
      let genesis = emptyWorld { _wHero = commoner { _hPos = (0,0) } }
      let walkRight = Walk Right
      let expectedAfterWalkRight = emptyWorld { _wHero = commoner { _hPos = (1,0) } }
      let actualAfterWalkRight = updateWorld walkRight genesis
      expectedAfterWalkRight `shouldBe` actualAfterWalkRight
      let walkLeft = Walk Left
      let expectedAfterWalkLeft = emptyWorld { _wHero = commoner { _hPos = (-1,0) } }
      let actualAfterWalkLeft = updateWorld walkLeft genesis
      expectedAfterWalkLeft `shouldBe` actualAfterWalkLeft
      let walkUp = Walk Up
      let expectedAfterWalkUp = emptyWorld { _wHero = commoner { _hPos = (0,-1) } }
      let actualAfterWalkUp = updateWorld walkUp genesis
      expectedAfterWalkUp `shouldBe` actualAfterWalkUp
      let walkDown = Walk Down
      let expectedAfterWalkDown = emptyWorld { _wHero = commoner { _hPos = (0,1) } }
      let actualAfterWalkDown = updateWorld walkDown genesis
      expectedAfterWalkDown `shouldBe` actualAfterWalkDown
