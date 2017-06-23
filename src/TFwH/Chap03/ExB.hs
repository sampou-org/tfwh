-- |
-- = 第3章 練習問題 B
--
-- * @(^^)@ はどのように定義するか
--
--   @
--   import Prelude hiding ((^^))
--
--   (^^) :: (Fractional a, Integral b) => a -> b -> a
--   x ^^ p = if p < 0 then recip (x ^ abs p) else x ^ p
--   @
module TFwH.Chap03.ExB where

import Prelude hiding ((^^))

-- |
--
-- >>> 2.5 ^^ 2
-- 6.25
-- >>> 2.5 ^^ (-2)
-- 0.16
--
(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ p = if p < 0 then recip (x ^ abs p) else x ^ p
