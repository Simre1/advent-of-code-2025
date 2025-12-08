module Day8.Main where

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
import Linear.V3
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [V3 Int]

distance :: V3 Int -> V3 Int -> Double
distance a b = case b - a of V3 x y z -> sqrt $ fromIntegral (x ^ 2 + y ^ 2 + z ^ 2)

computeDistancesNaively :: [V3 Int] -> [(Double, V3 Int, V3 Int)]
computeDistancesNaively junctions = L.sort [(distance a b, a, b) | a <- junctions, b <- junctions, a < b]

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day8/1"
  pure $ fmap read . (\[x, y, z] -> V3 x y z) . LS.splitOn "," <$> lines file

connectCircuit :: V3 Int -> V3 Int -> S.Set (S.Set (V3 Int)) -> S.Set (S.Set (V3 Int))
connectCircuit a b circuits =
  let (withA, withoutA) = S.partition (S.member a) circuits
      (withB, withoutB) = S.partition (S.member b) circuits
      merged = mconcat $ S.toList withA ++ S.toList withB ++ [S.fromList [a, b]]
   in S.singleton merged <> S.intersection withoutA withoutB

solution1 :: Input -> IO ()
solution1 input = do
  let ds = computeDistancesNaively input
      connections = take 1000 ds
      circuits = foldl' (\acc (_, a, b) -> connectCircuit a b acc) S.empty connections
      sizes = L.sortOn (\a -> -a) (S.size <$> S.toList circuits)
  print $ product $ take 3 sizes

solution2 :: Input -> IO ()
solution2 input = do
  let connections = computeDistancesNaively input
      circuits = scanl (\acc (_, a, b) -> connectCircuit a b acc) (S.fromList $ S.singleton <$> input) connections
      incomplete = takeWhile ((> 1) . S.size) circuits
      (_, V3 x1 _ _, V3 x2 _ _) = connections !! (length incomplete - 1)
  print $ x1 * x2

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
