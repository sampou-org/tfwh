-- |
-- = 第4章 練習問題 C
--
--     * 以下の関数disjoint の定義を示せ．
--
--       @
--       disjoint :: (Ord a) => [a] -> [a] -> Bool
--       @
--
--       この関数は2 つの昇順リストを取り，共通要素がないかどうかを判定するものとする．
--
--
module TFwH.Chap04.ExC where

disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint [] ys = True
disjoint xs [] = True
disjoint xxs@(x:xs) yys@(y:ys)
  | x < y     = disjoint xs yys
  | x > y     = disjoint xxs ys
  | otherwise = False


