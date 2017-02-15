module Lib
  ( Piece(..)
  , Loc(..)
  , Level(..)
  , Action(..)
  , generateLevel
  ) where

import Data.Map as M

data Piece
  = Player
  | Wall
  | Monster
  deriving (Show, Eq)

data Loc = Loc
  { _locX :: Int
  , _locY :: Int
  } deriving (Show, Eq, Ord)

data Level = Level
  { _bCells :: M.Map Loc Piece
  , _bSize :: Int
  } deriving (Show, Eq)

data Action
  = Fight
  | Walk
  deriving (Show, Eq)

generateLevel :: Int -> Level
generateLevel size = Level { _bCells = M.empty, _bSize = size }
