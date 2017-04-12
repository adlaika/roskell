module Main where

import System.Console.ANSI
import System.IO
import Control.Monad (guard)
import Control.Monad.State.Lazy
import Data.Maybe

import Game
import Types
import Level
import Draw

main :: IO ()
main = do
  defaultSettings
  renderInitialState
  forever runGameLoop
    where
      genesis = emptyWorld { _wLevel = level1 }
      renderInitialState = runStateT getAndDrawWorld genesis
      runGameLoop = do
        (action, world) <- runStateT pureGameLoop genesis
        runIO action

defaultSettings :: IO ()
defaultSettings = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Roskell"
  clearScreen

runIO :: GameAction -> IO ()
runIO action@(GameInput next) = getChar >>= validInputOrTryAgain
  where
    validInputOrTryAgain c = runIO $ maybe action next $ charToMaybeInput c
    -- ^ if input is invalid, ask for another char and try again
runIO (GameLog string next) = putStr string >> runIO next
runIO (GameDraw world next) = drawWorld world >> runIO next
runIO (GameUpdate input world next) = runIO $ next newWorld
  where newWorld = updateWorld input world
runIO GameExit = handleQuitGame

handleQuitGame :: IO ()
handleQuitGame = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]
  putStrLn "Thank you for playing -- press CTRL-C to close!"
