-- |
-- 第1章 練習問題 A
-- 
module TFwH.Chap01.ExA where

import Prelude hiding (sum,concat)

-- |
-- 指定した整数の2倍
--
-- @
-- double x = 2 * x
-- @
--
-- >>> map double [1, 4, 4, 3]
-- [2,8,8,6]
-- >>> map (double . double) [1, 4, 4, 3]
-- [4,16,16,12]
-- >>> map double []
-- []
--
double :: Integer -> Integer
double x = 2 * x

-- |
-- 整数のリストの総和
--
-- @
-- sum [] = 0
-- sum (x:xs) = x + sum xs
-- @
--
-- @
-- 法則: sum . map double = double . sum
-- @
--
-- prop> (sum . map double) (xs :: [Integer]) == (double . sum) xs
-- 
-- @
-- [] の場合：
--   sum (map double [])
-- =   { mapの定義 }
--   sum []
-- =   { sumの定義 }
--   0
-- =   { 算術 }
--   2 * 0
-- =   { doubleの定義 }
--   double 0
-- =   { sumの定義 }
--   double (sum [])
--
-- x:xs の場合：
--   sum (map double (x:xs))
-- =   { mapの定義 }
--   sum (double x : map double (x:xs))
-- =   { sumの定義 }
--   double x + sum (map double xs)
-- =   { 帰納法の仮定 }
--   double x + double (sum xs)
-- =   { doubleの定義 }
--   2 * x + 2 * sum xs
-- =   { 分配則 }
--   2 * (x + sum xs)
-- =   { sumの定義 }
--   2 * (sum (x:xs))
-- =   { doubleの定義 }
--   double (sum (x:xs))
-- @
--
sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs

-- |
-- リストのリストを連結
--
-- @
-- concat [] = []
-- concat (xs:xss) = xs ++ concat xss
-- @
--
-- @
-- 法則: sum . map sum = sum . concat
-- @
--
-- @
-- [] の場合：
--   sum (map sum [])
-- =   { mapの定義 }
--   sum []
-- =   { concatの定義 }
--   sum (concat [])
--
-- xs:xss の場合
--   sum (map sum (xs:xss))
-- =   { mapの定義 }
--   sum (sum xs : map sum xss)
-- =   { sumの定義 }
--   sum xs + sum (map sum xss)
-- =   { 帰納法の仮定 }
--   sum xs + sum (concat xss)
-- =   { sum xs + sum ys = sum (xs ++ ys) だから (1)}
--   sum ( xs ++ concat xss)
-- =   { concatの定義 }
--   sum ( concat (xs:xss))
-- @
--
-- @
-- 主張(1) xs 上の帰納法
--
-- []の場合：
--   sum [] + sum ys
-- =  { sumの定義 }
--   0 + sum ys
-- =  { 0は(+)の単位元 }
--   sum ys
-- =  { []は(++)の単位元 }
--   sum ([] ++ ys)
--
-- x:xsの場合：
--   sum (x:xs) + sum ys
-- =   { sumの定義 }
--   (x + sum xs) + sum ys
-- =   { (+)の結合性 }
--   x + (sum xs + sum ys)
-- =   { 帰納法の仮定 }
--   x + sum (xs ++ ys)
-- =   { sum の定義 }
--   sum (x : (xs ++ ys))
-- =   { (++)の定義 }
--   sum ((x:xs) ++ ys)
-- @

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- |
-- 整列
--
-- @
-- sort [] = []
-- sort (x:xs) = insert x (sort xs)
-- @
--
-- @
-- 法則: sum . sort = sum
-- @
--
-- @
-- [] の場合：
--   sum (sort [])
-- =   { sortの定義 }
--   sum []
--
-- (x:xs)
--   sum (sort (x:xs))
-- =   { sortの仕様 }
--   sum (insert x (sort xs))
-- =   { sum (insert y ys) = y + sum ys だから (2)}
--   x + sum (sort xs)
-- =   { 帰納法の仮定 }
--   x + sum xs
-- =   { sumの定義 }
--   sum (x:xs)
-- @
--
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)

-- |
-- ソート済みのリストに新しい要素を挿入
--
-- @
-- insert x [] = [x]
-- insert x (y:ys) = if x <= y then x : y : ys
--                   else y : insert x ys
-- @
--
-- @
-- 主張(2) ys 上の帰納法
--
-- []の場合：
--   sum (insert x [])
-- =   { insertの定義 }
--   sum [x]
-- =   { sumの定義 }
--   x + sum []
--
-- y:ysの場合： x <= y ならば
--   sum (insert x (y:ys))
-- =   { insertの定義 }
--   sum (x:(y:ys))
-- =   { sumの定義 }
--   x + sum (y:ys)
--
-- y:ysの場合： x > y ならば
--   sum (insert x (y:ys))
-- =   { insertの定義 }
--   sum (y : insert x ys)
-- =   { sumの定義 }
--   y + sum (insert x ys)
-- =   { 帰納法の仮定 }
--   y + (x + sum ys)
-- =   { (+)の結合性 }
--   (y + x) + sum ys
-- =   { (+)の可換性 }
--   (x + y) + sum ys
-- =   { (+)の結合性 }
--   x + (y + sum ys)
-- =   { sumの定義 }
--   x + sum (y:ys)
-- @
--
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys
                  else y : insert x ys 

