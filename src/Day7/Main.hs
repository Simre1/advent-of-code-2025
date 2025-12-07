module Day7.Main where

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
import Debug.Trace
import Linear.V2
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = (Int, M.Map Int (S.Set Int))

readInput :: IO Input
readInput = parseFileMega "inputs/day7/1" $ do
  let loop acc@(s, splitterMap) = do
        P.choice
          [ do
              char <- P.satisfy (const True)
              case char of
                '^' -> do
                  P.SourcePos _ r c <- P.getSourcePos
                  loop
                    ( s,
                      M.alter
                        (Just . maybe (S.singleton (P.unPos c - 2)) (S.insert (P.unPos c - 2)))
                        (P.unPos r `quot` 2)
                        splitterMap
                    )
                'S' -> do
                  P.SourcePos _ _ c <- P.getSourcePos
                  loop (P.unPos c - 2, splitterMap)
                _ -> loop acc,
            acc <$ P.eof
          ]
  loop (undefined, M.empty)

splitBeams :: S.Set Int -> S.Set Int -> (Int, S.Set Int)
splitBeams beams splitters =
  let unobstructed = S.difference beams splitters
      hit = S.intersection beams splitters
      splitted = mconcat $ (\i -> S.fromList [i - 1, i + 1]) <$> S.toList hit
   in (S.size hit, splitted <> unobstructed)

travelDownwards :: Int -> M.Map Int (S.Set Int) -> (Int, S.Set Int)
travelDownwards start = M.foldl' (\(splits, beams) -> first (+ splits) . splitBeams beams) (0, S.singleton start)

splitBeamsTimeline :: M.Map Int Int -> S.Set Int -> M.Map Int Int
splitBeamsTimeline beams splitters =
  let (hit, unobstructed) = M.partitionWithKey (\k _ -> S.member k splitters) beams
      splitted = M.unionsWith (+) $ (\(i, t) -> M.fromList [(i - 1, t), (i + 1, t)]) <$> M.toList hit
   in M.unionWith (+) splitted unobstructed

travelDownwardsTimeline :: Int -> M.Map Int (S.Set Int) -> M.Map Int Int
travelDownwardsTimeline start = M.foldl' splitBeamsTimeline (M.singleton start 1)

solution1 :: Input -> IO ()
solution1 input = do
  let (splits, _) = uncurry travelDownwards input
  print splits

solution2 :: Input -> IO ()
solution2 input = do
  let timelines = uncurry travelDownwardsTimeline input
  print $ sum timelines

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
