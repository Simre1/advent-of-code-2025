module Day3.Main where

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
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [Bank]

newtype Bank = Bank [Int]

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day3/1"
  let banks = lines file
  pure $ Bank . fmap (read . pure) <$> banks

highestRating2 :: Bank -> Int
highestRating2 (Bank batteries) = go 0 0 batteries
  where
    go b1 b2 [] = b1 * 10 + b2
    go b1 b2 (x : xs)
      | x > b1 && not (null xs) = go x 0 xs
      | x > b2 = go b1 x xs
      | otherwise = go b1 b2 xs

highestRatingN :: Int -> Bank -> Int
highestRatingN n (Bank batteries) = go (replicate n 0) batteries (length batteries - 1)
  where
    go !bs (x : xs) remaining =
      let newBs = update bs x remaining (n - 1)
       in go newBs xs (remaining - 1)
    go !bs [] _ = sum $ zipWith (\b e -> b * (10 ^ e)) bs [n - 1, n - 2 .. 0]
    update (b : bs) x remaining need
      | x > b && need <= remaining = x : replicate need 0
      | otherwise = b : update bs x remaining (need - 1)
    update [] _ _ _ = []

solution1 :: Input -> IO ()
solution1 input = do
  banks <- readInput
  print $ sum $ highestRating2 <$> banks

solution2 :: Input -> IO ()
solution2 input = do
  banks <- readInput
  print $ sum $ highestRatingN 12 <$> banks

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
