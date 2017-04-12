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

import Prelude hiding (Either(..))
import Control.Monad.State.Lazy

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
    'w' -> return $ Walk Up
    's' -> return $ Walk Down
    'a' -> return $ Walk Left
    'd' -> return $ Walk Right
    _ -> Nothing

updateWorld :: Input -> World -> World
updateWorld (Walk dir) world = handleWalk dir world
updateWorld _ world = undefined

handleWalk :: Direction -> World -> World
handleWalk dir world = do
  let hero = _wHero world
  let (heroX, heroY) = _hPos hero |+| dirToCoord dir
  world { _wHero = hero { _hPos = (heroX, heroY) } }
