module Types
  ( Input(..)
  , InvalidMove(..)
  , World(..)
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
  , directionToCoord
  , emptyLevel
  , emptyWorld
  , tileToChar
  , (|+|)
  ) where

import Prelude hiding (Either(..))
import Data.Map as M

-- IO Types --

data Input
  = Walk Direction
  deriving (Eq, Show)

directionToCoord :: Direction -> Coord
directionToCoord DirUp = (0, -1)
directionToCoord DirDown = (0, 1)
directionToCoord DirLeft = (-1, 0)
directionToCoord DirRight = (1, 0)

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq, Show)

data InvalidMove
  = Collision Tile
  deriving (Eq, Show)

-- World State Types --

data World = World
  { _wHero :: Hero
  , _wLevel :: Level
  , _wTick :: Int
  } deriving (Eq, Show)

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
emptyWorld = World commoner emptyLevel 0
