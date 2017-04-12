module GameSpec where

import Prelude hiding (Either(..))
import Test.Hspec

import Types
import Game

spec :: Spec
spec =
  describe "updateWorld" $
    it "given a movement input and a world, should move the hero" $ do
      let oldHero = commoner { _hPos = (0,0) }
      let oldWorld = emptyWorld { _wHero = oldHero }
      let input = Walk Right
      let newHero = commoner { _hPos = (1,0) }
      let expected = emptyWorld { _wHero = newHero }
      let actual = updateWorld input oldWorld
      expected `shouldBe` actual
