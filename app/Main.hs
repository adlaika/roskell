module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad (guard)
import Control.Monad.State.Lazy

import Types
import Level

main = do
  defaultSettings
  let world = genesis { _wLevel = level1 }
  runStateT drawWorld world
  runStateT gameLoop world

drawChar :: Char -> IO ()
drawChar '\n' = putChar '\n'
drawChar '.' = drawCharNormalForeground Green '.'
drawChar '@' = drawCharNormalForeground Red '@'
drawChar '#' = drawCharNormalForeground Black '#'
drawChar '~' = drawCharNormalForeground Blue '~'
drawChar _ = drawCharNormalForeground Black ' '

-- "Vivid" v "Dull" appears to have no effect.
drawCharNormalForeground :: Color -> Char -> IO ()
drawCharNormalForeground = drawChar' NormalIntensity Foreground Vivid

drawChar' :: ConsoleIntensity -> ConsoleLayer -> ColorIntensity -> Color -> Char -> IO ()
drawChar' consoleIntensity layer colorIntensity color char = do
  setSGR
    [ SetConsoleIntensity consoleIntensity
    , SetColor layer colorIntensity color ]
  putChar char

drawCoord :: Coord -> WorldState IO ()
drawCoord coord = do
  liftIO $ uncurry (flip setCursorPosition) coord
  world <- get
  liftIO $ drawChar (coordToChar coord world)

drawWorld :: WorldState IO ()
drawWorld = do
  liftIO $ setCursorPosition 0 0
  world <- get
  let lvl = _wLevel
  let (x', y') = _lMax $ lvl world
  let chars world = [[coordToChar (x,y) world | x <- [0..x']] | y <- [0..y']]
  liftIO $ mapM_ drawChar (unlines $ chars world)

drawHero :: WorldState IO ()
drawHero = do
  world <- get
  let hero = _wHero world
  let newPos = _hCurrPos hero
  let oldPos = _hOldPos hero
  guard $ newPos == oldPos
  world <- get
  drawCoord newPos
  drawCoord oldPos

coordToChar :: Coord -> World -> Char
coordToChar coord (World hero lvl)
  | _hCurrPos hero == coord = '@'
  | otherwise = case getTile coord lvl of
    Just tile -> tileToChar tile
    Nothing -> error $ "No tile at " ++ show coord ++ "!"

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return QuitGame
    'w' -> return $ Walk Up
    's' -> return $ Walk Down
    'a' -> return $ Walk Left
    'd' -> return $ Walk Right
    _ -> getInput

-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleWalk :: Direction -> WorldState IO ()
handleWalk direction = do
  world <- get
  let hero = _wHero world
  let lvl = _wLevel world
  let (heroX, heroY) = _hCurrPos hero |+| dirToCoord direction
  let inBounds i = max 0 (min i 80)
  let newX = inBounds heroX
  let newY = inBounds heroY
  let coord = (newX, newY)
  if isWall coord lvl
    then do
      put World
        { _wHero = hero { _hOldPos = _hCurrPos hero }
        , _wLevel = lvl
        }
      gameLoop
    else do
      put World
        { _wHero = hero
          { _hOldPos  = _hCurrPos hero
          , _hCurrPos = coord }
        , _wLevel = lvl
        }
      gameLoop

gameLoop :: WorldState IO ()
gameLoop = do
  world <- get
  drawHero
  input <- liftIO getInput
  case input of
    QuitGame -> liftIO handleQuitGame
    (Walk dir) -> handleWalk dir

handleQuitGame :: IO ()
handleQuitGame = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]
  putStrLn "Thank you for playing!"

defaultSettings :: IO ()
defaultSettings = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Roskell"
  clearScreen
