-- |
-- = 第3章 練習問題 C
--
-- * @div@ を以下のように定義できるか
--
-- @
-- div :: Integral a => a -> a -> a
-- div x y = floor ( x / y)
-- @
--
-- 答： (/) は Integral クラスでは定義されていないのでできない．
--
module TFwH.Chap03.ExC where

import Prelude hiding (div)

-- |
-- @
-- div x y = floor (fromIntegral x / fromIntegral y)
-- @
-- >>> div 7 4
-- 1
--
div :: Integral a => a -> a -> a
div x y = floor (fromIntegral x / fromIntegral y)
