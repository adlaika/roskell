module Draw
  ( drawWorld
  , drawChar
  , getAndDrawWorld
  ) where

import System.Console.ANSI
    ( clearScreen,
      setCursorPosition,
      setSGR,
      Color(Blue, White, Red, Green),
      ColorIntensity(Vivid),
      ConsoleIntensity(NormalIntensity),
      ConsoleLayer(Foreground),
      SGR(SetColor, SetConsoleIntensity) )
import Control.Monad.State.Lazy

import Types
import Level

drawWorld :: World -> IO ()
drawWorld world = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  where
    lvl = _wLevel world
    (x', y') = _lMax lvl
    chars = [[coordToChar (x,y) world | x <- [0..x']] | y <- [0..y']]

getAndDrawWorld :: StateT World IO ()
getAndDrawWorld = do
  world <- get
  liftIO $ drawWorld world

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
