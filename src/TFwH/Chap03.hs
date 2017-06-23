-- |
-- = TTFwH 第3章 数値
--
module TFwH.Chap03
  ( -- * 3 数値
    -- ** 3.1 型クラスNum
    -- ** 3.2 名前と演算子
    -- ** 3.3 床値の計算
    floorS
  , floorS'
  , floor0n
  , floor0p
    -- *** 二分探索
  , floor1
  , shrink
  , choose
  , bound
  , lower
  , upper
    -- ** 3.4 自然数
  , Nat (..)
  ) where

import Prelude hiding (floor)
import Numeric


lt :: Float -> Integer -> Bool
x `lt` n = x < fromInteger n

leq :: Integer -> Float -> Bool
m `leq` x = fromInteger m <= x

-- |
-- 尻高版 floor
--
floorS :: Float -> Integer
floorS = read . takeWhile (/= '.') . show

-- |
-- 尻高君のアイデアを活かした floor
--
floorS' :: Float -> Integer
floorS' = read . takeWhile (/= '.') . flip (showFFloat Nothing) ""

-- |
-- 最初の素朴な版 floor (負値)
-- 
floor0n :: Float -> Integer
floor0n x = until (`leq` x) (subtract 1) (-1)

-- |
-- 最初の素朴な版 floor (非負値)
-- 
floor0p :: Float -> Integer
floor0p x = until (x `lt`)  (+ 1) 1 - 1

-- |
-- 最初の素朴な版 floor
--
floor0 x = if x < 0
  then until (`leq` x) (subtract 1) (-1)
  else until (x `lt`) (+ 1) 1 - 1

-- |
-- 二分探索版 floor
--
floor1 :: Float -> Integer
floor1 x = fst (until unit (shrink x) (bound x))
           where
             unit (m, n) = m + 1 == n

type Interval = (Integer, Integer)

-- |
-- 区間の縮小
shrink :: Float -> Interval -> Interval
shrink x (m, n) = if p `leq` x then (p, n) else (m, p)
                  where
                    p = choose (m, n)

-- |
-- 新しい境界の選択
choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2

-- |
-- 最初の区間
bound :: Float -> Interval
bound x = (lower x, upper x)

-- |
-- 最初の下側境界
lower :: Float -> Integer
lower x = until (`leq` x) (* 2) (-1)

-- |
-- 最初の上側境界
upper :: Float -> Integer
upper x = until (x `lt`) (*2) 1

-- |
-- 自然数
data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

-- |
-- Num クラスのインスタンス
--
instance Num Nat where
  m + Zero   = m
  m + Succ n = Succ (m + n)

  m * Zero   = Zero
  m * Succ n = m * n + m

  abs n = n

  signum Zero     = Zero
  signum (Succ n) = Succ Zero

  m - Zero        = m
  Zero - Succ n   = Zero
  Succ m - Succ n = m - n

  fromInteger x
    | x <= 0    = Zero
    | otherwise = Succ (fromInteger (x - 1))
