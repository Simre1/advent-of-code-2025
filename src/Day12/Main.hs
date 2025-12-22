module Day12.Main where

import Algorithm.Search
import Control.Applicative
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Char
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import GHC.Generics
import GHC.IO.Unsafe (unsafePerformIO)
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = (V6 Shape, [Region])

data V6 a = V6 a a a a a a deriving (Eq, Ord, Show, Functor, Generic)

instance (Num a) => Num (V6 a) where
  (+) (V6 a1 b1 c1 d1 e1 f1) (V6 a2 b2 c2 d2 e2 f2) =
    V6
      (a1 + a2)
      (b1 + b2)
      (c1 + c2)
      (d1 + d2)
      (e1 + e2)
      (f1 + f2)
  (*) (V6 a1 b1 c1 d1 e1 f1) (V6 a2 b2 c2 d2 e2 f2) =
    V6
      (a1 * a2)
      (b1 * b2)
      (c1 * c2)
      (d1 * d2)
      (e1 * e2)
      (f1 * f2)
  abs = fmap abs
  signum = fmap signum
  fromInteger a = fromInteger <$> V6 a a a a a a
  negate = fmap negate

instance Foldable V6 where
  foldMap h (V6 a b c d e f) = h a <> h b <> h c <> h d <> h e <> h f

newtype Shape = Shape (S.Set (V2 Int)) deriving (Eq, Ord, Show)

data Region = Region (V2 Int) (V6 Int) deriving (Eq, Ord, Show)

indexV6 :: V6 a -> Int -> a
indexV6 (V6 a b c d e f) i = case i of
  0 -> a
  1 -> b
  2 -> c
  3 -> d
  4 -> e
  5 -> f
  _ -> error "out ouf bounds for V6"

setV6 :: V6 a -> Int -> a -> V6 a
setV6 (V6 a b c d e f) i x = case i of
  0 -> V6 x b c d e f
  1 -> V6 a x c d e f
  2 -> V6 a b x d e f
  3 -> V6 a b c x e f
  4 -> V6 a b c d x f
  5 -> V6 a b c d e x
  _ -> error "out ouf bounds for V6"

greedySearch ::
  (Ord cost, Eq state) =>
  (state -> [(state, cost)]) ->
  (state -> Bool) ->
  state ->
  ([state], Bool)
greedySearch nextStates isGoal initial = go initial
  where
    go !state = do
      let next = L.sortOn snd $ nextStates state
      case next of
        ((x, _) : xs) ->
          if isGoal x
            then ([x], True)
            else first (x :) (go x)
        _ -> ([], False)

isPlacable :: V6 Shape -> Region -> ([S.Set (V2 Int)], Bool)
isPlacable shapes (Region size@(V2 sizeX sizeY) counts) =
  first (fmap fst) $
    greedySearch
      nextStates
      isGoal
      initial
  where
    totalArea = product size
    setOrientations = S.toList . allOrientations <$> shapes
    nextStates (!free, !remaining) = do
      i <- [0 .. 5]
      Shape shape <- indexV6 setOrientations i
      let shapeRemaining = indexV6 remaining i
      guard $ shapeRemaining > 0
      V2 x y <- worthyPlacements free
      let newPlacedPositions = (+ V2 x y) `S.map` shape
      guard $ newPlacedPositions == S.intersection free newPlacedPositions
      let !worthOfPlaced =
            getSum $
              foldMap
                (\freePosition -> Sum $ 0.6 ^ occupiedVicinity free freePosition)
                newPlacedPositions
      let !nextFree = free S.\\ newPlacedPositions
      let !cost = worthOfPlaced
          !newRemaining = setV6 remaining i (shapeRemaining - 1)
      pure ((nextFree, newRemaining), cost)
    worth :: S.Set (V2 Int) -> Double
    worth set =
      getSum $
        foldMap
          (\freePosition -> Sum $ 0.6 ^ occupiedVicinity set freePosition)
          set
    worthyPlacements free = filter (\pos -> occupiedVicinity free pos > 0) $ S.toList free
    isBorder (V2 x y) = x == 0 || x == sizeX - 1 || y == 0 || y == sizeY - 1
    estimate :: V6 Int -> Double
    estimate remaining = fromIntegral (sum (remaining * shapeSizes)) * coefficient
    shapeSizes = (\(Shape shape) -> S.size shape) <$> shapes
    averageShapeSize = fromIntegral (sum shapeSizes) / 6
    coefficient = fromIntegral (product size) / fromIntegral (sum shapeSizes) :: Double
    isGoal (_, remaining) = sum remaining == 0
    initial = (S.fromList $ V2 <$> xs <*> ys, counts)
    neighbors (V2 x y) =
      let offsets = filter (/= V2 0 0) $ V2 <$> [-1, 0, 1] <*> [-1, 0, 1]
       in (+ V2 x y) <$> offsets
    xs = [0 .. sizeX - 1]
    ys = [0 .. sizeY - 1]
    occupiedVicinity set pos = length $ filter (`S.notMember` set) (neighbors pos)

allOrientations :: Shape -> S.Set Shape
allOrientations (Shape set) =
  S.fromList $
    fmap
      Shape
      [ set,
        flipX set,
        flipY set,
        flipX $ flipY set,
        rotate90 set,
        rotate90 $ rotate90 set,
        rotate90 $ rotate90 $ rotate90 set
      ]
  where
    flipX :: S.Set (V2 Int) -> S.Set (V2 Int)
    flipX = S.map (\pos -> V2 1 0 + V2 (-1) 1 * (pos - V2 1 0))
    flipY :: S.Set (V2 Int) -> S.Set (V2 Int)
    flipY = S.map (\pos -> V2 0 1 + V2 1 (-1) * (pos - V2 0 1))
    rotate90 :: S.Set (V2 Int) -> S.Set (V2 Int)
    rotate90 =
      S.map
        ( \pos ->
            let (V2 x y) = pos - V2 1 1
             in V2 (-y) x
        )

readInput :: IO Input
readInput = parseFileMega "inputs/day12/1" $ do
  [s0, s1, s2, s3, s4, s5] <- many shape
  regions <- many region
  pure (V6 s0 s1 s2 s3 s4 s5, regions)
  where
    shape = P.try $ do
      _ <- P.satisfy isDigit
      lexemeFull $ P.char ':'
      row1 <- lexemeFull $ P.takeP Nothing 3
      row2 <- lexemeFull $ P.takeP Nothing 3
      row3 <- lexemeFull $ P.takeP Nothing 3
      let token = row1 <> row2 <> row3
          word =
            foldl'
              ( \acc i ->
                  if T.index token i == '#'
                    then
                      S.insert
                        (V2 (i `mod` 3) (i `quot` 3))
                        acc
                    else acc
              )
              S.empty
              [0 .. 8]
      pure $ Shape word
    region = do
      x <- P.decimal
      P.char 'x'
      y <- P.decimal
      lexemeFull $ P.char ':'
      [s0, s1, s2, s3, s4, s5] <- lexemeFull $ many $ lexeme P.decimal
      pure $ Region (V2 x y) (V6 s0 s1 s2 s3 s4 s5)

isPlacable2 :: V6 Shape -> Region -> Bool
isPlacable2 shapes (Region size counts) = product size >= sum (fmap (\(Shape set) -> S.size set) shapes * counts)

solution1 :: Input -> IO ()
solution1 (shapes, regions) =
  print $ length $ filter (isPlacable2 shapes) regions

solution2 :: Input -> IO ()
solution2 (shapes, regions) = do
  results <-
    traverse
      ( \region -> do
          let (free, res) = isPlacable shapes region
          mapM print2DSet free
          print res
          pure res
      )
      regions
  print $ length $ filter id results

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
