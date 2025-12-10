{-# LANGUAGE TypeAbstractions #-}

module Day10.Main where

import Algorithm.Search (aStar, bfs)
import Control.Applicative
import Control.Monad
import Control.Monad (guard)
import Control.Monad.ST
import Data.Bits
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.SBV
import Data.SBV.Dynamic (CV (..))
import Data.SBV.Internals (CVal (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VMU
import Data.Word
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [(Word32, V.Vector (VU.Vector Int), VU.Vector Int)]

readInput :: IO Input
readInput = parseFileMega "inputs/day10/1" $ many $ do
  i <- indicator
  b <- buttons
  j <- joltage
  P.newline
  pure $ (i, b, j)
  where
    indicator = lexeme $ do
      P.char '['
      lights <- many $ P.satisfy (\c -> c == '#' || c == '.')
      let positions = mapMaybe (\(i, c) -> if c == '#' then Just i else Nothing) $ zip [0 ..] lights
      P.char ']'
      pure $ makeBitMask positions
    buttons = fmap V.fromList $ many $ lexeme $ do
      P.char '('
      toggledLights <- lexeme $ P.sepBy numberP (P.char ',')
      P.char ')'
      pure (VU.fromList toggledLights)
    joltage = lexeme $ do
      P.char '{'
      joltages <- lexeme $ P.sepBy (fromIntegral <$> numberP) (P.char ',')
      P.char '}'
      pure (VU.fromList joltages)

naiveSearch :: V.Vector (VU.Vector Int) -> Word32 -> [Word32]
naiveSearch buttons goal = fromJust $ bfs nextState (== goal) 0
  where
    nextState state = (state `xor`) <$> buttons'
    buttons' = V.map (makeBitMask . fmap fromIntegral . VU.toList) buttons

naiveJoltageSearch :: V.Vector (VU.Vector Int) -> VU.Vector Int -> (Int, [VU.Vector Int])
naiveJoltageSearch buttons goal =
  fromJust $
    aStar
      nextState
      cost
      estimate
      isGoal
      startState
  where
    startState = VU.map (const 0) goal
    isGoal = (== goal)
    cost _ _ = 1
    estimate state = VU.maximum $ VU.zipWith (-) goal state
    nextState :: VU.Vector Int -> V.Vector (VU.Vector Int)
    nextState state = flip fmap buttons $ \b ->
      runST $ \ @s -> do
        mut <- VU.thaw state
        VU.forM_ b $ \i ->
          VMU.modify mut (+ 1) i
        VU.freeze mut

toEquations :: V.Vector (VU.Vector Int) -> VU.Vector Int -> [([Int], Int)]
toEquations buttons joltages = do
  zip [0 ..] (VU.toList joltages) <&> \(i, j) ->
    let usefulButtons = mapMaybe (\(bI, b) -> if VU.elem i b then Just bI else Nothing) (zip [0 ..] (V.toList buttons))
     in (usefulButtons, j)

makeBitMask :: [Int] -> Word32
makeBitMask = foldl' setBit 0

solution1 :: Input -> IO ()
solution1 input =
  print $ sum $ length . (\(goal, buttons, _) -> naiveSearch buttons goal) <$> input

solution2 :: Input -> IO ()
solution2 input = do
  let input' = fmap (\(_, b, j) -> (b, j)) input
      equations = uncurry toEquations <$> input'

  results <- forM equations $ \equation -> do
    result <- optimize Lexicographic (model equation)
    case result of
      LexicographicResult smt -> case getModelObjectiveValue "total-x-sum" smt of
        Just mi -> case mi of
          RegularCV cv -> case cvVal cv of
            CInteger i -> pure i
            _ -> error "Not possible"
          _ -> error "Not possible"
        _ -> error "Not possible"
  print $ sum results

-- do
--   result <- sat model
--   print result
model :: [([Int], Int)] -> Symbolic ()
model equations = do
  let maxVar = maximum (concatMap fst equations)
  xs <- V.fromList <$> sIntegers ["x" ++ show i | i <- [0 .. maxVar]]

  let makeConstraint (idxs, s) = sum [xs V.! i | i <- idxs] .== literal (fromIntegral s)
  mapM_ (constrain . makeConstraint) equations
  mapM_ (\x -> constrain (x .>= 0)) xs
  minimize "total-x-sum" (sum xs)

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
