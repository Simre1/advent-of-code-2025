module Day2.Main where

import Advent (ordNub)
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad (guard)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (nub)
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
import Text.Read (readMaybe)

type Input = [(Int, Int)]

readInput :: IO Input
readInput = do
  file <- T.readFile "inputs/day2/1"
  let ranges = T.split (== ',') file
  pure $
    ranges <&> \range -> case T.split (== '-') range of
      [l, r] -> (read $ T.unpack l, read $ T.unpack r)

findRepeatedPattern :: (Int, Int) -> [Int]
findRepeatedPattern (li, ri) = do
  lHalf <- maybeToList makelHalf
  rHalf <- maybeToList makerHalf
  half <- [lHalf .. rHalf]
  let num = read $ show half ++ show half
  guard $ li <= num && num <= ri
  [num]
  where
    l = show li
    r = show ri
    makelHalf, makerHalf :: Maybe Int
    makelHalf = readMaybe $ take (length l `quot` 2) l
    makerHalf = readMaybe $ take (length r `quot` 2) r

isRepeatTwice :: Int -> Bool
isRepeatTwice i = (i `quot` x) == i `mod` x
  where
    x = 10 ^ (n `quot` 2)
    n = ceiling $ logBase 10 (fromIntegral i)

isRepeating :: Int -> Bool
isRepeating i = or $ do
  n <- [1 .. maxN]
  let b = 10 ^ n
  let patterns = generate i b
  guard $ length patterns * length (show $ head patterns) == length (show i)
  pure $ length patterns > 1 && length (nub patterns) == 1
  where
    maxN = ceiling $ logBase 10 (fromIntegral i)

generate num b =
  let q = num `quot` b
      m = num `mod` b
   in if q == 0
        then [m]
        else m : generate q b

solution1 :: Input -> IO ()
solution1 input = do
  let invalidIds = mconcat $ input <&> \(l, r) -> filter isRepeatTwice [l, l + 1 .. r]
  print $ sum invalidIds

-- print $ for input $ \(l, r) -> [l, l + 1 .. r]

solution2 :: Input -> IO ()
solution2 input = do
  let invalidIds = mconcat $ input <&> \(l, r) -> filter isRepeating [l, l + 1 .. r]
  print $ sum invalidIds

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
