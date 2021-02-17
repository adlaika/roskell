module Level
    ( stringsToLevel
    , level1
    , getTile
    , isWall
    , isArmor
    , isWeapon
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Types

safeMax :: Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax xs = Just $ maximum xs

stringsToLevel :: [String] -> Level
stringsToLevel strs = foldl populate emptyLevel {_lMax=maxXY} asciiMap
  where
    asciiMap = concat $ zipWith zip coords strs
    coords = [[(x, y) | x <- [0..]] | y <- [0..]]
    xs = map getX asciiMap
    getX = fst . fst
    maxFromMaybe l = fromMaybe 0 $ safeMax l
    maxX = maxFromMaybe xs
    ys = map getY asciiMap
    getY = snd . fst
    maxY = maxFromMaybe ys
    maxXY = (maxX, maxY)
    populate lvl (coord, tile) =
      case tile of
        '#' -> lvl { _lTiles = M.insert coord Wall t }
        '.' -> lvl { _lTiles = M.insert coord Empty t }
        '~' -> lvl { _lTiles = M.insert coord Water t }
        _   -> lvl
        where t = _lTiles lvl

getTile :: Coord -> Level -> Maybe Tile
getTile coord lvl = M.lookup coord (_lTiles lvl)

map1 :: [String]
map1   = [ "##############################"
         , "#............#################"
         , "#............############....#"
         , "#............................#"
         , "#......~~~~~.................#"
         , "#.....~~~~~~......##.........#"
         , "#.......~~~.....#####........#"
         , "##############################" ]

level1 :: Level
level1 = stringsToLevel map1

isWall :: Coord -> Level -> Bool
isWall coord lvl = case M.lookup coord (_lTiles lvl) of
  Just Wall -> True
  _ -> False

isArmor :: Coord -> Level -> Bool
isArmor coord lvl = case M.lookup coord (_lItems lvl) of
  Just (ItemArmor _) -> True
  _ -> False

isWeapon :: Coord -> Level -> Bool
isWeapon coord lvl = case M.lookup coord (_lItems lvl) of
  Just (ItemWeapon _) -> True
  _ -> False
