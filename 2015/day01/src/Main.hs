{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)

type Input = [Int]

part1 :: Input -> Int
part1 = sum

part2 :: Input -> Maybe Int
part2 = elemIndex (-1) . scanl (+) 0

prepare :: String -> Input
prepare = mapMaybe (\case
  '(' -> pure 1
  ')' -> pure (-1)
  _ -> Nothing)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
