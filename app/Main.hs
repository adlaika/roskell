module Main where

import Console
import Types
import Level

main = do
  defaultSettings
  let world = genesis { _wLevel = level1 }
  drawWorld world
  gameLoop world
