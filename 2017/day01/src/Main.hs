{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt, isDigit)
import Data.Maybe (mapMaybe)

type Input = [Int]

filterIndex :: (a -> Int -> Bool) -> [a] -> [a]
filterIndex predicate xs = [x | (x, i) <- zip xs [0..], predicate x i]

-- filter by (== array[n + 1 || 0 if n == length - 1]) -> sum
part1 :: Input -> Int
part1 xs = sum $ filterIndex (\x i -> x == xs !! (if i + 1 >= length xs then 0 else i + 1)) xs

part2 :: Input -> Int
part2 xs = sum $ filterIndex (\x i -> x == xs !! ((i + length xs `div` 2) `mod` length xs)) xs

prepare :: String -> Input
prepare = mapMaybe (\c -> if isDigit c then Just (digitToInt c) else Nothing)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
