{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import Data.List (group, sort)

newtype Coordinate = Coordinate (Int, Int) deriving (Show, Eq, Ord)
type Input = [Coordinate]

instance Semigroup Coordinate where
  Coordinate (a, b) <> Coordinate (c, d) = Coordinate (a + c, b + d)

instance Monoid Coordinate where
  mempty = Coordinate (0, 0)

part1 :: Input -> Int
part1 = length . group . sort . scanl (<>) (Coordinate (0, 0))

filterIdx :: (Int -> Bool) -> [a] -> [a]
filterIdx predicate = map snd . filter (predicate . fst) . zip [0..]

part2 :: Input -> Int
part2 = length . group . sort . concatMap (scanl (<>) (Coordinate (0, 0))) . split'
  where split' xs = [filterIdx even xs, filterIdx odd xs]

prepare :: String -> Input
prepare = mapMaybe (\case
  '^' -> pure $ Coordinate (0, -1) 
  '>' -> pure $ Coordinate (1, 0)
  'v' -> pure $ Coordinate (0, 1)
  '<' -> pure $ Coordinate (-1, 0)
  _ -> Nothing)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
