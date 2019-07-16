module Game
  ( GameAction(..)
  , gameInput
  , gameLog
  , gameDraw
  , gameExit
  , gameUpdate
  , pureGameLoop
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

gameInput :: (Input -> GameAction) -> GameAction
gameInput = GameInput

gameLog :: String -> GameAction -> GameAction
gameLog = GameLog

gameDraw :: World -> GameAction -> GameAction
gameDraw = GameDraw

gameExit :: GameAction
gameExit = GameExit

gameUpdate :: Input -> World -> (World -> GameAction) -> GameAction
gameUpdate = GameUpdate

pureGameLoop :: Monad m => GameState m GameAction
pureGameLoop =
  get >>= \oldWorld ->
  return $ gameInput $ \input -> -- get/parse player input
  gameUpdate input oldWorld $ \newWorld -> -- update the world based on above
  gameDraw newWorld $ -- draw the updated world
  if input == QuitGame
    then gameExit
    else evalState pureGameLoop newWorld

charToMaybeInput :: Char -> Maybe Input
charToMaybeInput char =
  case char of
    'q' -> return QuitGame
    'w' -> return $ Walk DirUp
    's' -> return $ Walk DirDown
    'a' -> return $ Walk DirLeft
    'd' -> return $ Walk DirRight
    _ -> Nothing

updateWorld :: Input -> World -> Either InvalidMove World
updateWorld (Walk dir) world = handleWalk dir world
updateWorld _ world = undefined

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
