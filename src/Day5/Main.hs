module Day5.Main where

import Control.Applicative
import Control.Monad
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Range = Range Int Int deriving (Eq, Show, Ord)

type Input = ([Range], [Int])

readInput :: IO Input
readInput = do
  parseFileMega "inputs/day5/1" $ do
    ranges <- many $ P.try $ do
      left <- numberP
      P.char '-'
      right <- numberP
      P.newline
      pure (Range left right)
    P.newline
    ingredients <- many $ lexemeFull numberP
    pure (ranges, ingredients)

withinRange :: [Range] -> Int -> Bool
withinRange ranges i = any (\(Range l r) -> l <= i && i <= r) ranges

solution1 :: Input -> IO ()
solution1 (ranges, ingredients) = do
  let fresh = filter (withinRange ranges) ingredients
  print $ length fresh

mergeRanges :: [Range] -> [Range]
mergeRanges = go . L.sort
  where
    go [] = []
    go [r] = [r]
    go (Range l1 r1 : Range l2 r2 : rs)
      | r1 < l2 = Range l1 r1 : go (Range l2 r2 : rs)
      | r1 == l2 = go (Range l1 r2 : rs)
      | otherwise = go (Range l1 (max r1 r2) : rs)

countElements :: [Range] -> Int
countElements = foldl' (\acc (Range l r) -> acc + (r - l + 1)) 0

solution2 :: Input -> IO ()
solution2 (ranges, _) = do
  print $ countElements $ mergeRanges ranges

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
