module Day6.Main where

import Control.Applicative
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid (Sum (..))
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

type Input = String

data Op = Mult | Add deriving (Eq, Ord, Show)

toOp :: String -> Op
toOp "*" = Mult
toOp "+" = Add

readInput :: IO Input
readInput = readFile "inputs/day6/1"

readNormal :: Input -> [(Op, [Int])]
readNormal file =
  let l = lines file
      numbers = L.transpose $ fmap read . words <$> init l
      ops = fmap toOp . words $ last l
   in zip ops numbers

readCephalopod :: Input -> [(Op, [Int])]
readCephalopod file =
  let l = lines file
      numbers = fmap read <$> LS.splitWhen (all (== ' ')) (L.transpose $ init l)
      ops = fmap toOp . words $ last l
   in zip ops numbers

applyOp :: Op -> [Int] -> Int
applyOp Mult = product
applyOp Add = sum

solution1 :: Input -> IO ()
solution1 input = do
  print $ foldMap (Sum . uncurry applyOp) (readNormal input)

solution2 :: Input -> IO ()
solution2 input = do
  print $ foldMap (Sum . uncurry applyOp) (readCephalopod input)

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
