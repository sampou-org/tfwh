-- |
-- = 第3章 練習問題 A
--
-- 
-- @3 + -2@ と @2 + subtract 3@ はエラー
-- 
-- >>> -2 + 3
-- 1
-- >>> 3 + (-2)
-- 1
-- >>> subtract 2 3
-- 1
--
module TFwH.Chap03.ExA where

import Prelude hiding (subtract)

-- |
--
-- @
-- import Prelude hiding (subtract)
--
-- subtract :: Num a => a -> a -> a
-- subtract = flip (-)
-- @
--
-- >>> subtract 2 3
-- 1
--
subtract :: Num a => a -> a -> a
subtract = flip (-)
