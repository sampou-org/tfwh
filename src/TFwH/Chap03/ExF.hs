-- |
-- = 第3章 練習問題 F
--
-- * y が √x の近似であるとき，x / y も √x の近似であるが，これらより良い近似は何か．
--
-- 答： (y + x / y) / 2
--
-- * 絶対誤差検査と相対誤差検査のどちらが適切か．
--
-- 答： 相対誤差検査
--
-- * @sqrt :: Float -> Float@ を定義せよ．
--
-- @
-- sqrt :: Float -> Float
-- sqrt x = until goodenough improve x
--   where
--     goodenough y = abs (y * y - x) < eps * x
--     improve y = (y + x / y) / 2
--     eps = 0.000001
-- @

module TFwH.Chap03.ExF where

import Prelude hiding (sqrt)
-- |
-- Float の平方根
sqrt :: Float -> Float
sqrt x = until goodenough improve x
  where
    goodenough y = abs (y * y - x) < eps * x
    improve y = (y + x / y) / 2
    eps = 0.000001

-- |
-- Float の平方根
-- 相対誤差の閾値 eps として Float の機械イプシロンを使う
sqrt' :: Float -> Float
sqrt' x = until goodenough improve x
  where
    goodenough y = abs (y * y - x) <= meps * x
    improve y = (y + x / y) / 2

-- |
-- Floatに対する機械イプシロン
--
meps :: Float
meps = until (\ x -> 1 + x == 1) (/ 2) (1 :: Float)
