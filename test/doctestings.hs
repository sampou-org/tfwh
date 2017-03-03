module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "src/TFwH/Chap01/ExA.hs"
  , "src/TFwH/Chap01/ExB.hs"
  , "src/TFwH/Chap01/ExC.hs"
  , "src/TFwH/Chap01/ExD.hs"
  , "src/TFwH/Chap01/ExE.hs"
  ]
