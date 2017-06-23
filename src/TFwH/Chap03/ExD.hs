-- |
-- = 第3章 練習問題 D
--
-- * 尻高君の floor では上手くいかない理由は科学記法による浮動小数点数表記以外に何があるか．
--
-- @
-- floor :: Float -> Integer
-- floor = read . takeWhile (/= '.') . show
-- @
--
-- 答： floor (-3.1) を評価すると -4 ではなく -3 になる．
--
module TFwH.Chap03.ExD where

import Numeric

-- |
-- 尻高君の floor
--
floorS :: Float -> Integer
floorS = read . takeWhile (/= '.') . show

-- |
-- 尻高君のアイデアを無理に活かした実装．Numeric モジュールをインポートして showFFloat を使えばなんとなるかな．
-- 
-- @
-- floorS' :: Float -> Integer
-- floorS' x = if "0." == take 2 (reverse s) || 0 < x then r else r-1
--   where
--     s = showFFloat Nothing x ""
--     t = takeWhile (/= '.') s
--     r = read t
-- @
--
-- >>> floorS' (12345678.0 :: Float)
-- 12345678
-- >>> floorS' (-3.1)
-- -4
-- >>> floorS' (-3.0)
-- -3

floorS' :: Float -> Integer
floorS' x = if "0." == take 2 (reverse s) || 0 < x then r else r-1
  where
    s = showFFloat Nothing x ""
    t = takeWhile ('.' /=) s
    r = read t
