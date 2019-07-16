module Level
    ( stringsToLevel
    , levelToStrings
    , level1
    , getTile
    , isWall
    , isArmor
    , isWeapon
    , coordToChar
    ) where

import qualified Data.Map as M

import Data.List.Index (imap)
import Data.Maybe (fromMaybe)

import Types

coordToChar :: Coord -> World -> Char
coordToChar coord (World hero lvl)
  | _hPos hero == coord = '@'
  | otherwise = case getTile coord lvl of
    Just tile -> tileToChar tile
    Nothing -> error $ "No tile at " ++ show coord ++ "!"

safeMax :: Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax xs = Just $ maximum xs

stringsToLevel :: [String] -> Level
stringsToLevel asciiStrings = foldl populate genesis asciiMap
  where
    genesis = emptyLevel { _lMax=maxXY }
      where
        maxXY = (maxFromMaybe xs, maxFromMaybe ys)
        maxFromMaybe = fromMaybe 0 . safeMax
        xs = map (fst . fst) asciiMap
        ys = map (snd . fst) asciiMap
    asciiMap = concat $ zipWith zip coords asciiStrings
      where
        coords = [[(x, y) | x <- [0..]] | y <- [0..]]
    populate lvl (coord, tile) =
      case tile of
        '#' -> lvl { _lTiles = M.insert coord Wall  t }
        '.' -> lvl { _lTiles = M.insert coord Empty t }
        '~' -> lvl { _lTiles = M.insert coord Water t }
        _   -> lvl
        where t = _lTiles lvl

levelToStrings :: Level -> [String]
levelToStrings lvl = ascii
  where
    ascii = M.foldrWithKey tileToAscii (replicate (maxY + 1) "") tileStrings
      where
        maxY = snd (_lMax lvl)
        tileStrings = fmap show tiles
        tiles = _lTiles lvl
    tileToAscii (x,y) tile = imap addTile
      where
        addTile yIndex line = if yIndex == y && x <= maxX
          then tile ++ line
          else line
        maxX = fst (_lMax lvl)
    -- maxXY = (2,2)
    -- x y tile   ascii
    -- -------------------------------
    -- 0 0 "#" -> ["#",   "",    ""]
    -- 1 0 "." -> ["#.",  "",    ""]
    -- 2 0 "." -> ["#..", "",    ""]
    -- 0 1 "." -> ["#..", ".",   ""]
    -- 1 1 "#" -> ["#..", ".#",  ""]
    -- 2 1 "~" -> ["#..", ".#~", ""]
    -- 0 2 "." -> ["#..", ".#~", "."]
    -- 1 2 "#" -> ["#..", ".#~", ".#"]
    -- 2 2 "." -> ["#..", ".#~", ".#."]


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
