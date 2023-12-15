{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Text.Regex.Applicative (RE, (=~), sym, psym, many)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.List (sort)

type Input = [Present]

data Present = Present { len, width, height :: Int } deriving (Show)

type Parser a = RE Char a

number :: Parser Int
number = read <$> many (psym isDigit)

surfaceArea :: Present -> Int
surfaceArea p = 2*(len p * width p + width p * height p + height p * len p)

smallestSide :: Present -> [Int]
smallestSide p = take 2 $ sort [len p, width p, height p]

part1 :: Input -> Int
part1 = sum . map (\p -> smallest p + surfaceArea p)
  where smallest = product . smallestSide

part2 :: Input -> Int
part2 = sum . map (\p -> ribbon p + volume p)
  where ribbon = (*2) . sum . smallestSide 
        volume p = product $ [len p, width p, height p]

prepare :: String -> Input
prepare = mapMaybe (=~ parsePresent) . lines
  where parsePresent :: Parser Present
        parsePresent = Present <$> (number <* sym 'x') <*> (number <* sym 'x') <*> number

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
