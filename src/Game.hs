module Game
  ( GameAction(..)
  , tick
  , charToMaybeInput
  , updateWorld
  ) where

import qualified Data.Map as M

import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe)

import Types

data GameAction
  = GameInput (Input -> GameAction)
  | GameLog String GameAction
  | GameDraw World GameAction
  | GameUpdate Input World (World -> GameAction)
  | GameExit

-- tickDo :: Monad m => StateT World m GameAction
-- tickDo = do
--   oldWorld <- get
--   undefined 

tick :: Monad m => StateT World m GameAction
tick =
  get >>= \oldWorld ->
  pure $ GameInput $ \input -> -- get/parse player input
  GameUpdate input oldWorld $ \newWorld -> -- update the world based on above
  GameDraw newWorld $ -- draw the updated world
  evalState tick newWorld

charToMaybeInput :: Char -> Maybe Input
charToMaybeInput char =
  case char of
    'w' -> return $ Walk DirUp
    's' -> return $ Walk DirDown
    'a' -> return $ Walk DirLeft
    'd' -> return $ Walk DirRight
    _ -> Nothing

updateWorld :: Input -> World -> Either InvalidMove World
updateWorld (Walk dir) world = handleWalk dir world

handleWalk :: Direction -> World -> Either InvalidMove World
handleWalk direction world = do
  let hero = _wHero world
  let oldPosition = _hPos hero
  let newPosition = oldPosition |+| directionToCoord direction
  if immovableAt newPosition
    then Left  $ Collision (tileAt newPosition)
    else Right $ world { _wHero = hero { _hPos = newPosition } }
    where
      immovableAt pos = case tileAt pos of
        Wall -> True
        _ -> False
      tileAt pos = fromMaybe
        Empty
        (M.lookup pos tiles)
      tiles = _lTiles $ _wLevel world
