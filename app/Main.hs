module Main where

import Day1.Main qualified as D1
import Day10.Main qualified as D10
import Day11.Main qualified as D11
import Day12.Main qualified as D12
import Day2.Main qualified as D2
import Day3.Main qualified as D3
import Day4.Main qualified as D4
import Day5.Main qualified as D5
import Day6.Main qualified as D6
import Day7.Main qualified as D7
import Day8.Main qualified as D8
import Day9.Main qualified as D9
import System.Environment (getArgs)

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of
    "1" -> D1.main
    "2" -> D2.main
    "3" -> D3.main
    "4" -> D4.main
    "5" -> D5.main
    "6" -> D6.main
    "7" -> D7.main
    "8" -> D8.main
    "9" -> D9.main
    "10" -> D10.main
    "11" -> D11.main
    "12" -> D12.main
    _ -> print "No puzzle matched"
