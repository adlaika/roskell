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
stringsToLevel str = foldl populate emptyLevel {_lMax=maxXY} asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords = [[(x, y) | x <- [0..]] | y <- [0..]]
    xs = map (fst . fst) asciiMap
    maxFromMaybe l = fromMaybe 0 $ safeMax l
    maxX = maxFromMaybe xs
    ys = map (snd . fst) asciiMap
    maxY = maxFromMaybe ys
    maxXY = (maxX, maxY)
    populate lvl (coord, tile) =
      case tile of
        '#' -> lvl { _lTiles = M.insert coord Wall t }
        '.' -> lvl { _lTiles = M.insert coord Empty t }
        '~' -> lvl { _lTiles = M.insert coord Water t }
        _   -> lvl
        where t = _lTiles lvl

levelToStrings :: Level -> [String]
levelToStrings lvl = ascii
  where
    maxX = fst (_lMax lvl)
    xs = take maxX [0..]
    maxY = snd (_lMax lvl)
    ys = take maxY [0..]
    tiles :: M.Map Coord Tile
    tiles = _lTiles lvl
    tileStrings :: M.Map Coord String
    tileStrings = fmap show tiles
    ascii :: [String]
    ascii = M.foldrWithKey tileToAscii (replicate (maxY + 1) "") tileStrings
    tileToAscii :: Coord -> String -> [String] -> [String]
    tileToAscii (x,y) tile = imap addTile
      where
        addTile yIndex line = if yIndex == y && x <= maxX
          then tile ++ line
          else line
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
