{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (isPrefixOf, find)

type Input = String

findPrefixMD5 :: String -> String -> Maybe Int
findPrefixMD5 prefix str = snd <$> find ((prefix `isPrefixOf`) . fst) [(md5' (str ++ show x), x) | x :: Int <- [0..]]
  where md5' = show . md5 . LB.pack

part1 :: Input -> Maybe Int
part1 = findPrefixMD5 "00000"

part2 :: Input -> Maybe Int
part2 = findPrefixMD5 "000000"

main :: IO ()
main = print $ (part1 &&& part2) "ckczppom"
