-- |
-- = 第4章 練習問題 E
--
module TFwH.Chap04.ExA where

import Data.Function
import Data.List
import Data.Ord

pairs :: [((Int,Int),(Int,Int))]
pairs = mergeAll
      $ map (sortBy (comparing (fst.fst)))
      $ [[((x^3+y^3,s),(x,y)) | let s' = s`div`2, x <- [1..s'], let y=s-x] | s <- [2..]]

mergeAll ((xs:xss):yss:zsss)
  = xs : mergeAll (merge xss yss:zsss)

merge [] yss = yss
merge xss [] = xss
merge xxs@(x:xs) yys@(y:ys) = case comparing (fst.fst) x y of
  GT -> y:merge xxs ys
  _  -> x:merge xs yys

ramanujans = filter ((2<) . length) $ groupBy ((==) `on` (fst . fst)) pairs

