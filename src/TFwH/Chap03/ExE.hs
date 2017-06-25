-- |
-- = 第3章 練習問題 E
--
-- * @isqrt :: Float -> Integer@ は非負数の平方根の床を返す関数である．
--   3.3 節の戦略に従って，@isqrt x@ を log x ステップで計算する実装を構成せよ．
--
-- @
-- isqrt :: Float -> Integer
-- isqrt x = fst (until unit (shrink x) (bound x))
--   where
--     unit (m,n) = m + 1 == n
--    
-- shrink :: Float -> Interval -> Interval
-- shrink x (m, n) = if (p * p) `leq` x then (p, n) else (m, p)
--   where
--     p = (m + n) `div` 2
--
-- bound :: Float -> Interval
-- bound x = (0, until above (* 2) 1)
--   where
--     above n = x `lt` (n * n)
-- @

module TFwH.Chap03.ExE where

type Interval = (Integer, Integer)

-- |
-- 非負数の平方根の床を返す
--
-- >>> isqrt 123456.78
-- 351
--
isqrt :: Float -> Integer
isqrt x = fst (until unit (shrink x) (bound x))
  where
    unit (m,n) = m + 1 == n

-- |
-- 区間の縮小
--
shrink :: Float -> Interval -> Interval
shrink x (m, n) = if (p * p) `leq` x then (p, n) else (m, p)
  where
    p = (m + n) `div` 2

-- |
-- 最初の区間
bound :: Float -> Interval
bound x = (0, until above (* 2) 1)
  where
    above n = x `lt` (n * n)

-- |
-- ≦
--
leq :: Integer -> Float -> Bool
m `leq` x = fromIntegral m <= x

-- |
-- ＜
--
lt :: Float -> Integer -> Bool
x `lt` n = x < fromIntegral n
