module Main where


import Types
import Level
import IO

main :: IO ()
main = do
  defaultSettings
  let world = genesis { _wLevel = level1 }
  _ <- drawWorld world
  gameLoop world
