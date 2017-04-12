module Types
  ( Input(..)
  , World(..)
  , GameState
  , Coord
  , Level(..)
  , Hero(..)
  , HP(..)
  , Tile(..)
  , Item(..)
  , Armor(..)
  , Weapon(..)
  , Direction(..)
  , commoner
  , dirToCoord
  , emptyLevel
  , emptyWorld
  , tileToChar
  , (|+|)
  ) where

import Prelude hiding (Either(..))
import Data.Map as M
import Control.Monad.State.Lazy

-- IO Types --

data Input
  = Fight Direction
  | Walk Direction
  | QuitGame
  deriving (Eq)

dirToCoord :: Direction -> Coord
dirToCoord Up = (0, -1)
dirToCoord Down = (0, 1)
dirToCoord Left = (-1, 0)
dirToCoord Right = (1, 0)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq)

-- World State Types --

data World = World
  { _wHero :: Hero
  , _wLevel :: Level
  } deriving (Eq, Show)

type GameState = StateT World

type Coord = (Int, Int)

-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Level = Level
  { _lTiles :: M.Map Coord Tile
  , _lItems :: M.Map Coord Item
  , _lMax :: Coord
  } deriving (Eq, Show)

-- Entity Types --

data Hero = Hero
  { _hPos :: Coord
  , _hHP :: HP
  , _hArmor :: Armor
  , _hWeapon :: Weapon
  } deriving (Eq, Show)

data Item
  = ItemArmor Armor
  | ItemWeapon Weapon
  deriving (Eq, Show)

data Armor = Armor
  { _aDefense :: Defense
  , _aName :: Name } deriving (Eq, Show)

data Weapon = Weapon
  { _wAttack :: Attack
  , _wName :: Name } deriving (Eq, Show)

newtype HP = HP Int deriving (Eq, Show)
newtype Attack = Attack Int deriving (Eq, Show)
newtype Defense = Defense Int deriving (Eq, Show)
newtype Name = Name String deriving (Eq, Show)

data Tile
  = Empty
  | Wall
  | Water
  deriving (Eq)

instance Show Tile where
  show Empty = "."
  show Wall = "#"
  show Water = "~"

tileToChar :: Tile -> Char
tileToChar tile = case show tile of
  [x] -> x
  _:_ -> error "calling show on a tile produced a string with more than one char."
  [] -> error "calling show on a tile produced an empty string"

-- Utility functions --

commoner :: Hero
commoner = Hero
  { _hPos = (1,1)
  , _hHP = HP 10
  , _hArmor = rags
  , _hWeapon = fists
  }

rags :: Armor
rags = Armor (Defense 0) (Name "Rags")

fists :: Weapon
fists = Weapon (Attack 1) (Name "Fists")

emptyLevel :: Level
emptyLevel = Level M.empty M.empty (0,0)

emptyWorld :: World
emptyWorld = World commoner emptyLevel
