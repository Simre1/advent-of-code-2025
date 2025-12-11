module Day11.Main where

import Control.Applicative
import Control.Monad
import Control.Monad (guard)
import Data.Char
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

newtype Device = Device Text deriving (Eq, Ord, Show)

type Input = M.Map Device (S.Set Device)

readInput :: IO Input
readInput = parseFileMega "inputs/day11/1" $ fmap M.fromList $ many $ do
  dev <- device
  _ <- lexeme $ P.char ':'
  outputs <- many1 device
  P.newline
  pure (dev, S.fromList outputs)
  where
    device = lexeme $ P.try $ do
      dev <- P.takeP (Just "device") 3
      guard $ T.all isAlphaNum dev
      pure $ Device dev

newtype PathNum = PathNum (M.Map Visit Int) deriving (Eq, Ord, Show)

data Visit = None | Dac | Fft | Both deriving (Eq, Ord, Show)

instance Num PathNum where
  fromInteger n = PathNum $ M.singleton None (fromInteger n)
  (PathNum m1) + (PathNum m2) = PathNum $ M.unionWith (+) m1 m2
  (PathNum m1) * (PathNum m2) = PathNum $ M.unionWith (*) m1 m2
  abs (PathNum m) = PathNum $ abs <$> m
  signum (PathNum m) = PathNum $ signum <$> m
  negate (PathNum m) = PathNum $ negate <$> m

mark :: Visit -> Visit -> PathNum -> PathNum
mark v1 v2 (PathNum m) = PathNum $ case M.lookup v1 m of
  Nothing -> m
  Just i -> M.alter (Just . maybe i (+ i)) v2 $ M.delete v1 m

-- markFft :: PathNum -> PathNum
-- markFft

-- markDac :: PathNum -> PathNum

countPaths :: Input -> Device -> Int
countPaths connections dev
  | dev == Device "out" = 1
  | otherwise = case M.lookup dev connections of
      Nothing -> 0
      Just nextDevices ->
        sum $ countPaths connections <$> S.toList nextDevices

countPaths2 :: Input -> Device -> PathNum
countPaths2 connections dev = cache M.! dev
  where
    cache :: M.Map Device PathNum
    cache = M.fromList $ (Device "out", 1) : ((\d -> (d, go d)) <$> M.keys connections)
    go :: Device -> PathNum
    go dev = case M.lookup dev connections of
      Nothing -> 0
      Just nextDevices ->
        let nextPaths = sum $ (cache M.!) <$> S.toList nextDevices
         in if
              | dev == Device "fft" -> mark None Fft $ mark Dac Both nextPaths
              | dev == Device "dac" -> mark None Dac $ mark Fft Both nextPaths
              | otherwise -> nextPaths

solution1 :: Input -> IO ()
solution1 input = print $ countPaths input (Device "you")

solution2 :: Input -> IO ()
solution2 input = do
  let PathNum m = countPaths2 input (Device "svr")
  print $ m M.! Both

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
