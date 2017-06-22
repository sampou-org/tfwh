{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- = 第2章 練習問題 F
-- 
--    * 以下は x の n 乗を求める関数で，この関数では exp x n を求めるのに n-1 回の乗算が必要です．
--
--        @
--        exp :: Integer -> Integer -> Integer
--        exp x n | n == 0    = 1
--                | n == 1    = x
--                | otherwise = x * exp x (n - 1)
--        @
--
--    * 尻高君の方法は以下のとおりで，乗算は高々 2p 回です．
--
--        @
--        exp :: Integer -> Integer -> Integer
--        exp x n | n == 0    = 1
--                | n == 1    = x
--                | even n    = exp (x * x) m
--                | odd n     = x * exp (x * x) m
--        @
--
--
--    * 関数呼び出しトレース
--
--      以下はオマケ(説明はありません)．
--
module TFwH.Chap02.ExF where

import Control.Monad.Fix
import Data.List
import Debug.Trace

-- |
-- O(n) 回の乗算を必要とする expG を生成する汎関数
--
gexpG :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
gexpG f x n | n == 0    = 1
            | n == 1    = x
            | otherwise = x * f x (n - 1)

-- |
-- O(n) 回の乗算を必要とする exp
--
-- >>> expG 2 13
-- 8192
--
expG  :: Integer -> Integer -> Integer
expG  = fix gexpG

-- |
-- O(n) 回の乗算を必要とする exp のトレース版
--
-- >>> expG' 2 13
-- exp 2 13
-- exp 2 12
-- exp 2 11
-- exp 2 10
-- exp 2 9
-- exp 2 8
-- exp 2 7
-- exp 2 6
-- exp 2 5
-- exp 2 4
-- exp 2 3
-- exp 2 2
-- exp 2 1
-- 8192
--
expG' :: Integer -> Integer -> Integer
expG' = tracing "exp" gexpG

-- |
-- O(log n) 回の乗算を必要とする expS を生成する汎関数
--
gexpS :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer)
gexpS f x n | n == 0  = 1
            | n == 1  = x
            | even n  = f (x * x) (n `div` 2)
            | odd  n  = x * f (x * x) (n `div` 2)

-- |
-- O(log n) 回の乗算を必要とする exp
--
-- >>> expS 2 13
-- 8192
--
expS  :: Integer -> Integer -> Integer
expS  = fix gexpS

-- |
-- O(log n) 回の乗算を必要とする exp のトレース版
--
-- >>> expS' 2 13
-- exp 2 13
-- exp 4 6
-- exp 16 3
-- exp 256 1
-- 8192
--
expS' :: Integer -> Integer -> Integer
expS' = tracing "exp" gexpS

-- |
-- 関数呼び出しの簡単なトレースを生成
--
traceOfCall :: (Show a, Show b) => String -> (a -> b -> c) -> a -> b -> c
traceOfCall s f x n = trace msg f x n
  where
    msg = intercalate " " [s,show x,show n]

-- |
-- トレースを追加する関数
--
tracing :: (Show a, Show b) => String -> ((a -> b -> c) -> a -> b -> c) -> a -> b -> c
tracing = gfun . traceOfCall

-- |
-- 機能 f を g に追加するした汎関数の不動点を求める
--
gfun :: ((a -> b) -> (c -> d)) -> ((c -> d) -> (a -> b)) -> (c -> d)
gfun f g = fix (f . g)
