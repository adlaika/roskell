module Main where

import IO
import Types
import Level

main = do
  defaultSettings
  let world = genesis { _wLevel = level1 }
  drawWorld world
  gameLoop world
