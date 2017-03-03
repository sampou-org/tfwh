module Main where

import System.Environment
import TFwH.OneManWentToMow

main :: IO ()
main = do
  { args <- getArgs
  ; let n = case args of { [] -> 3; a:_ -> read a `mod` 10}
  ; putStr $ song n
  }
