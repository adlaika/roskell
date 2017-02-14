module Lib
  ( Piece
  , Loc
  , Board 
  ) where

import Data.Aeson

data Piece
  = Player
  | Wall
  deriving (Show, Eq)

data Loc = Loc
  { _locX :: Int
  , _LocY :: Int
  } deriving (Show, Eq)

data Board = Board
  { _bCells :: (M.Map Loc Piece)
  , _bSize :: Int
  } deriving (Show, Eq)
