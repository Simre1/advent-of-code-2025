{-# LANGUAGE QuasiQuotes #-}

module Day9.Main where

import Control.Applicative
import Control.Monad
import Control.Monad (guard)
import Data.Bimap qualified as BM
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Debug.Trace
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [V2 Int]

-- 4548962670 -> too high
-- 4548962670

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day9/1"
  pure $ (\[x, y] -> V2 x y) . fmap read . LS.splitOn "," <$> lines file

rectSizes :: [V2 Int] -> [Int]
rectSizes ps = [mult (abs (b - a)) | a <- ps, b <- ps, a < b]

mult (V2 x y) = (x + 1) * (y + 1)

makeEdges :: [V2 Int] -> V2 (M.Map Int (S.Set (V2 Int)))
makeEdges ps = foldl' toLine (pure M.empty) ((last ps, head ps) : zip ps (tail ps))
  where
    toLine :: V2 (M.Map Int (S.Set (V2 Int))) -> (V2 Int, V2 Int) -> V2 (M.Map Int (S.Set (V2 Int)))
    toLine (V2 vertical horizontal) (V2 x1 y1, V2 x2 y2)
      | x1 == x2 =
          let yLine = if y1 > y2 then V2 y2 y1 else V2 y1 y2
           in V2 (M.alter (Just . maybe (S.singleton yLine) (S.insert yLine)) x1 vertical) horizontal
      | y1 == y2 =
          let xLine = if x1 > x2 then V2 x2 x1 else V2 x1 x2
           in V2 vertical (M.alter (Just . maybe (S.singleton xLine) (S.insert xLine)) y1 horizontal)
      | otherwise = error "No lines"

zipWithV2 :: (a -> b -> c) -> V2 a -> V2 b -> V2 c
zipWithV2 f (V2 x1 y1) (V2 x2 y2) = V2 (f x1 x2) (f y1 y2)

getLines :: V2 (M.Map Int (S.Set (V2 Int))) -> V2 Int -> V2 ([V2 Int], Maybe (V2 Int), [V2 Int])
getLines (V2 vertical horizontal) (V2 x y) =
  let (left, middle, right) = M.splitLookup x vertical
      (bottom, center, top) = M.splitLookup y horizontal
      f i c ls = catMaybes $ M.elems $ withinRange i <$> ls
   in V2
        (f y x left, middle >>= withinRange y, f y x right)
        (f x y bottom, center >>= withinRange x, f x y top)

intersect :: V2 (M.Map Int (S.Set (V2 Int))) -> V2 Int -> V2 Int -> V2 Int -> Bool
intersect (V2 vertical horizontal) (V2 cx cy) (V2 x1 y1) (V2 x2 y2)
  | x1 == x2 = intersect' horizontal cx (reorder $ V2 y1 y2) x1
  | y1 == y2 = intersect' vertical cy (reorder $ V2 x1 x2) y2
  where
    reorder (V2 a b) = if a < b then V2 a b else V2 b a
    makeSmaller (V2 a b) = (V2 (a + 1) (b - 1))
    intersect' :: M.Map Int (S.Set (V2 Int)) -> Int -> V2 Int -> Int -> Bool
    intersect' lines center (V2 a b) i =
      let (_, lines') = M.split a lines
          (lines'', _) = M.split b lines'
          ls = mconcat $ M.elems lines''
          intersections = S.filter (\(V2 a b) -> a <= i && i <= b) ls
          skewed = i + signum (center - i)
       in -- in traceShow (intersections, skewed, center) $ S.null $ S.filter (\(V2 a b) -> a <= skewed && skewed <= b) intersections
          S.null $ S.filter (\(V2 a b) -> a <= skewed && skewed <= b) intersections

withinRange i set = case S.lookupLT (V2 i maxBound) set of
  Nothing -> Nothing
  Just (V2 start end) -> if i <= end then Just (V2 start end) else Nothing

toSVG :: [V2 Int] -> String
toSVG (V2 x y : ps) =
  [__i|
    <svg
     width="100000"
     height="100000"
    >
    <path
      style="fill:\#000000;stroke-width:1;stroke:\#000000;stroke-opacity:1"
      d="M #{x} #{y} #{toPaths ps} Z"
    />
    </svg>
  |]
  where
    toPaths :: [V2 Int] -> String
    toPaths (V2 x y : ps) =
      [__i|
        L #{x} #{y}
    |]
        ++ toPaths ps
    toPaths _ = ""

rects :: [V2 Int] -> [(V2 Int, V2 Int)]
rects ps = [(a, b) | a <- ps, b <- ps, a < b]

solution1 :: Input -> IO ()
solution1 input =
  print $ maximum $ rectSizes input

check :: V2 (M.Map Int (S.Set (V2 Int))) -> (V2 Int, V2 Int) -> Bool
check edges (V2 x1 y1, V2 x2 y2) =
  let rectLines = [(V2 x1 y1, V2 x2 y1), (V2 x2 y1, V2 x2 y2), (V2 x2 y2, V2 x1 y2), (V2 x1 y2, V2 x1 y1)]
      rectCenter = V2 ((x2 + x1) `quot` 2) ((y2 + y1) `quot` 2)
   in all (uncurry (intersect edges rectCenter)) rectLines

pointCheck :: ([V2 Int], Maybe (V2 Int), [V2 Int]) -> V2 Int
pointCheck (l, m, r) = case m of
  Just _ -> if even (length l) then V2 (length l + 1) (length r) else V2 (length l) (length r + 1)
  Nothing -> V2 (length l) (length r)

solution2 :: Input -> IO ()
solution2 input = do
  let edges = makeEdges input
  print $ maximum $ (\(a, b) -> (mult . fmap abs $ b - a, a, b)) <$> filter (check edges) (rects input)

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
