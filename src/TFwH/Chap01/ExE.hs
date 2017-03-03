{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TFwH.Chap01.ExE where

import Data.Typeable
type Fun = Int -> Int
instance Show Fun where
  show = show . typeOf

-- ^
-- prop> (x + y) + z == (x :: Int) + (y + z)
-- prop> 0 + y == (y :: Int)
-- prop> x + 0 == (x :: Int)
-- prop> (xs ++ ys) ++ zs == (xs :: String) ++ (ys ++ zs)
-- prop> [] ++ ys == (ys :: String)
-- prop> xs ++ [] == (xs :: String)
-- prop> (((f::Fun) . (g::Fun)) . (h::Fun)) (x::Int) == (f . (g . h)) x
-- prop> (id . g) x == (g :: Fun) (x :: Int)
-- prop> (f . id) x == (f :: Fun) (x :: Int)

