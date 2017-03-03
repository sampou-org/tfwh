module Main where

import System.Environment
import TFwH.Anagrams (anagrams)

main :: IO ()
main = do
  { args <- getArgs
  ; let n = case args of { [] -> 6; a:_ -> read a }
  ; interact (anagrams n . words)
  }
