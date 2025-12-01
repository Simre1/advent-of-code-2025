module Template where

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
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = ()

readInput :: IO Input
readInput = error "inputs/dayX/input"

solution1 :: Input -> IO ()
solution1 input = pure ()

solution2 :: Input -> IO ()
solution2 input = pure ()

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
