module IO 
    ( getInput
    , defaultSettings
    , gameLoop
    , drawChar
    , drawChar'
    , drawCharNormalForeground
    , drawCoord
    , drawHero
    , drawWorld
    ) where

import System.Console.ANSI
import System.IO
import Types
import Level

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return QuitGame
    'w' -> return $ Walk North
    's' -> return $ Walk South
    'a' -> return $ Walk East
    'd' -> return $ Walk West
    _ -> getInput

-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleWalk :: World -> Direction -> IO ()
handleWalk world direction
  | isWall coord lvl = gameLoop world
      { _wHero = hero { _hOldPos = _hCurrPos hero } }
  | otherwise = gameLoop world
      { _wHero = hero { _hOldPos  = _hCurrPos hero, _hCurrPos = coord } }
  where
    hero = _wHero world
    lvl = _wLevel world
    coord = (newX, newY)
    newX = inBounds heroX
    newY = inBounds heroY
    (heroX, heroY) = _hCurrPos hero |+| dirToCoord direction
    inBounds i = max 0 (min i 80)

gameLoop :: World -> IO ()
gameLoop world = do
  drawHero world
  input <- getInput
  case input of
    QuitGame -> handleQuitGame
    (Walk dir) -> handleWalk world dir

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

coordToChar :: Coord -> World -> Char
coordToChar coord (World hero lvl)
  | _hCurrPos hero == coord = '@'
  | otherwise = case getTile coord lvl of
    Just tile -> tileToChar tile
    Nothing -> error $ "No tile at " ++ show coord ++ "!"

drawChar :: Char -> IO ()
drawChar '\n' = putChar '\n'
drawChar '.' = drawCharNormalForeground Green '.'
drawChar '@' = drawCharNormalForeground Red '@'
drawChar '#' = drawCharNormalForeground White '#'
drawChar '~' = drawCharNormalForeground Blue '~'
drawChar _ = drawCharNormalForeground White ' '

-- "Vivid" v "Dull" appears to have no effect.
drawCharNormalForeground :: Color -> Char -> IO ()
drawCharNormalForeground = drawChar' NormalIntensity Foreground Vivid

drawChar' :: ConsoleIntensity -> ConsoleLayer -> ColorIntensity -> Color -> Char -> IO ()
drawChar' consoleIntensity layer colorIntensity color char = do
  setSGR
    [ SetConsoleIntensity consoleIntensity
    , SetColor layer colorIntensity color ]
  putChar char

drawCoord :: World -> Coord -> IO ()
drawCoord world coord = do
  uncurry (flip setCursorPosition) coord
  drawChar (coordToChar coord world)

drawWorld :: World -> IO ()
drawWorld world = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  where
    lvl = _wLevel world
    (x', y') = _lMax lvl
    chars = [[coordToChar (x,y) world | x <- [0..x']] | y <- [0..y']]

drawHero :: World -> IO ()
drawHero world
  | newPos == oldPos = return ()
  | otherwise = do
    drawCoord world newPos
    drawCoord world oldPos
    where
      hero = _wHero world
      newPos = _hCurrPos hero
      oldPos = _hOldPos  hero
