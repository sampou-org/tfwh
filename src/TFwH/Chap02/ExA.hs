-- |
-- 第1章 練習問題 A
--
-- 問：2足す2の半分は2かそれとも3か
-- 答：そのとおり
--
module TFwH.Chap02.ExA where

-- |
--
-- @
-- problem' = 2 + (2 `div` 2)
-- @
--
problem' :: Int
problem' = 2 + (2 `div` 2)

-- |
--
-- @
-- problem'' = (2 + 2) `div` 2
-- @
--
problem'' :: Int
problem'' = (2 + 2) `div` 2

-- |
--
-- @
-- answer' = 3
-- @
--
answer' :: Int
answer' = 3

-- |
--
-- @
-- answer'' = 2
-- @
--
answer'' :: Int
answer'' = 2

-- |
--
-- @
-- answer = (problem'  == answer' || problem'  == answer'' )
--       && (problem'' == answer' || problem'' == answer'' )
-- @
--
-- >>> answer
-- True
answer :: Bool
answer =  (problem'  == answer' || problem'  == answer'' )
       && (problem'' == answer' || problem'' == answer'' )

