module Day4.Main where

import Control.Applicative
import Control.Monad
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Massiv.Array qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = M.Array M.S M.Ix2 Cell

type Cell = Word8

pattern Empty :: Cell
pattern Empty = 0

pattern Paper :: Cell
pattern Paper = 1

pattern Inaccessible :: Cell
pattern Inaccessible = 0

readInput :: IO Input
readInput =
  parseFileMega
    "inputs/day4/1"
    ( parseStorableMatrix $ \case
        '@' -> Paper
        _ -> Empty
    )

convolve :: (Cell -> [Cell] -> a) -> Input -> M.Array M.DW M.Ix2 a
convolve collect = M.mapStencil (M.Fill Empty) stencil
  where
    stencil = M.makeStencil (M.Sz2 3 3) (M.Ix2 1 1) $ \get ->
      collect (get (M.Ix2 0 0)) $ fmap (\(x, y) -> get (M.Ix2 x y)) combinations

    combinations = filter (/= (0, 0)) $ (,) <$> [-1, 0, 1] <*> [-1, 0, 1]

solution1 :: Input -> IO ()
solution1 input = do
  let accessible = M.computeS @M.S $ convolve (\middle cells -> if middle == Paper && sum cells < 4 then 1 :: Int else 0) input
  print $ M.sum accessible

solution2 :: Input -> IO ()
solution2 input = do
  let makeNext = M.computeS @M.S . convolve (\middle cells -> if sum cells < 4 then 0 :: Word8 else middle)
      left =
        M.iterateUntil
          (\_ -> (==))
          (\_ -> makeNext)
          input
      removed = fromJust $ input M..-. left
      asum = M.foldlS (\acc a -> fromIntegral a + acc) (0 :: Int)
  print $ asum input - asum left

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
