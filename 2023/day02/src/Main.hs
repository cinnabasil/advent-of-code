module Main where

import Control.Arrow ((&&&))
import Control.Applicative (asum, Alternative)

import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Data.Map.Strict qualified as M 
import Text.Regex.Applicative (RE, (=~), string, sym, many)
import Text.Regex.Applicative.Common (decimal)

data Game = Game Int [Pull] deriving (Show, Eq, Ord)

data Color = Red | Green | Blue deriving (Show, Eq, Ord)

newtype Pull = Pull (M.Map Color Int) deriving (Show, Eq, Ord)

instance Semigroup Pull where
  Pull m1 <> Pull m2 = Pull $ M.mergeWithKey (\_ x y -> Just (x + y)) id id m1 m2
instance Monoid Pull where
  mempty = Pull M.empty

type Parser a = RE Char a

type Input = [Game]

part1 :: Input -> Int
part1 = sum . map getGameId . filter possible
  where possible g = count g Red <= 12 && count g Green <= 13 && count g Blue <= 14 
        count (Game _ pulls) col = M.findWithDefault 0 col (foldl findMaxPulls M.empty pulls)
        findMaxPulls m1 (Pull m2) = M.mergeWithKey (\_ x y -> Just (max x y)) id id m1 m2
        getGameId (Game n _) = n

part2 :: Input -> ()
part2 = const ()

color :: Parser Color
color = asum [c <$ string (map toLower (show c)) | c <- [Red, Green, Blue]]

sepBy :: Alternative f => f a -> f b -> f [a]
p `sepBy` sep = (:) <$> p <*> many (sep *> p)

oneColorPull :: Parser Pull
oneColorPull = singleton <$> (decimal <* sym ' ') <*> color
  where singleton n c = Pull $ M.singleton c n

pull :: Parser Pull
pull = mconcat <$> (oneColorPull `sepBy` string ", ")

game :: Parser Game
game = Game <$> (string "Game " *> decimal) <* string ": " <*> pull `sepBy` string "; "

prepare :: String -> Input
prepare = mapMaybe (=~ game) . lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
