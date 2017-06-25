-- |
-- = 第3章 練習問題 G
--
-- * Nat を型クラスOrd のインスタンスとして定義せよ．そのうえで，以下のdivMod の定義を構成せよ．
-- @divMod :: Nat -> Nat -> (Nat, Nat)@
--
-- @
-- instance Ord Nat where
--   Zero <= Zero   = True
--   Zero <= Succ n = True
--   Succ m <= Zero = False
--   Succ m <= Succ n = m <= n
--
-- divMod :: Nat -> Nat -> (Nat, Nat)
-- divMod x y = if x < y then (Zero, x)
--              else (Succ q, r)
--   where
--     (q, r) = divMod (x - y) y
-- @
--
module TFwH.Chap03.ExG where

import Prelude hiding (divMod)

-- |
-- 自然数
--
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- |
-- 順序 Ord クラスのインスタンス
--
instance Ord Nat where
  Zero <= Zero   = True
  Zero <= Succ n = True
  Succ m <= Zero = False
  Succ m <= Succ n = m <= n

-- |
-- 数値 Num クラスのインスタンス
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

-- |
-- Nat 上の剰余算
--
divMod :: Nat -> Nat -> (Nat, Nat)
divMod x y = if x < y then (Zero, x)
             else (Succ q, r)
  where
    (q, r) = divMod (x - y) y
