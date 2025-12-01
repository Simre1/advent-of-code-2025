module Day1.Main where

import Control.Applicative
import Control.Monad
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (..))
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
import Text.Read (readMaybe)

data Rotation = L Int | R Int

type Input = [Rotation]

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day1/1"
  pure $
    flip mapMaybe (lines file) $ \line -> do
      let (rot, num) = splitAt 1 line
      makeRot <- case rot of
        "L" -> Just L
        "R" -> Just R
        _ -> Nothing
      num' <- readMaybe num
      pure $ makeRot num'

applyRotation :: Int -> Rotation -> Int
applyRotation i (L d) = (i - d) `mod` 100
applyRotation i (R d) = (i + d) `mod` 100

applyRotationV2 :: Int -> Rotation -> (Int, Int)
applyRotationV2 i rot
  | new > 0 = (new `quot` 100, new `mod` 100)
  | new == 0 = (1, 0)
  | new < 0 = (abs (new `quot` 100) + if i > 0 then 1 else 0, new `mod` 100)
  where
    new = case rot of
      L d -> i - d
      R d -> i + d

solution1 :: Input -> IO ()
solution1 input = do
  let start = 50
  rotations <- readInput
  let positions = L.scanl applyRotation start rotations
  print $ length $ filter (== 0) positions

solution2 :: Input -> IO ()
solution2 input = do
  let start = (0, 50)
  rotations <- readInput
  let positions = L.foldl (\(rs, num) -> first (+ rs) . applyRotationV2 num) start rotations
  -- print $ L.scanl (\(rs, num) -> first (+ rs) . applyRotationV2 num) start rotations
  print $ fst positions

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
