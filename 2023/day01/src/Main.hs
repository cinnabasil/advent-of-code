{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, find, tails)

type Input = [String]

part1 :: Input -> Int
part1 = sum . map (read . (\(x, y) -> [x, y]) . (head &&& last) . filter isDigit) . filter (/= "")

part2 :: Input -> Int
part2 = sum . map (makeNumber . (head &&& last) . mapMaybe extractNumber . tails) . filter (/= "")
  where extractNumber str = snd <$> find (\(name, _) -> name `isPrefixOf` str) digits
        makeNumber (x, y) = (x * 10) + y

digits :: [(String, Int)]
digits = [(show x, x) | x <- [1..9]] ++ zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1..9]

prepare :: String -> Input
prepare = lines

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
