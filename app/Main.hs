module Main where

import System.Console.ANSI
import System.IO
import Control.Monad ()
import Control.Monad.State.Lazy
import Data.Maybe ()

import Game
import Types
import Level
import Draw

main :: IO ()
main = do
  defaultSettings
  _ <- renderInitialState
  runGameLoop
    where
      genesis = emptyWorld { _wLevel = level1 }
      renderInitialState = runStateT getAndDrawWorld genesis
      runGameLoop = forever $ do
        (action, _) <- runStateT tick genesis
        runIO action

defaultSettings :: IO ()
defaultSettings = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Roskell"
  clearScreen

-- if input is invalid, ask for another char and try again
validInputOrTryAgain :: GameAction -> (Input -> GameAction) -> Char -> IO ()
validInputOrTryAgain thisAction nextAction char = do
  case charToMaybeInput char of
    Just x -> runIO $ nextAction x
    Nothing -> do 
      _ <- putStrLn "invalid input!"
      runIO thisAction 

runIO :: GameAction -> IO ()
runIO action@(GameInput next) = getChar >>= validInputOrTryAgain action next
runIO (GameLog string next) = do 
  -- setCursorColumn 0
  -- clearFromCursorToScreenEnd
  putStr string >> runIO next
runIO (GameDraw world next) = drawWorld world >> runIO next
runIO (GameUpdate input oldWorld next) = validUpdateOrOldWorld
  where validUpdateOrOldWorld = case updateWorld input oldWorld of
          Right w -> runIO $ next w
          -- Left (Collision _) -> do
            -- runIO $ GameLog "can't go there!" $ GameInput _
          Left (Collision _) -> putStrLn "can't go there!" >> runIO (next oldWorld)
          -- figure out how to ask for another input and try again, rather than waste a turn
runIO GameExit = handleQuitGame

handleQuitGame :: IO ()
handleQuitGame = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]
  putStrLn "Thank you for playing -- press CTRL-C to close!"
