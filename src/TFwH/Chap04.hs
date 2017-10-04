-- |
-- = TTFwH 第4章 リスト
--
module TFwH.Chap04
  ( -- * 4 リスト
    -- ** 4.1 リスト記法
    iterate
  , until
    -- $TranslationOfListComprehension
    -- ** 4.2 列挙
    -- ** 4.3 リスト内包表記
  , triads
  , divisors
  , coprime
  , coprime'
    -- ** 4.4 基本演算
  , null
  , head
  , tail
  , last
    -- ** 4.5 連接
  , (++)
  , length
    -- ** 4.6 concat, map, filter
  , concat
  , map
  , filter
    -- ** 4.7 zip と zipWith
  , zip
  , zipWith
  , nondec
  , nondec'
    -- ** 4.8 頻出単語（完成編）
  , commonWords
  , sort
  , msort
  ) where

import Prelude hiding (Word,iterate,until,null,head,tail,last
                      ,(++),length,concat,map,filter,zip,zipWith)
import Data.List (sortBy)
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Numeric

-- |
-- Prelude.iterate の定義例
--
-- @
-- iterate f x = x : iterate f (f x)
-- @
--
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- |
-- Prelude.until の定義例
--
-- @
-- until p f = head . filter p . iterate f
-- @
--
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f = head . filter p . iterate f

triads0 :: Int -> [(Int, Int, Int)]
triads0 n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n]
                      , x^2 + y^2 == z^2 ]
triads1 :: Int -> [(Int, Int, Int)]
triads1 n = [(x, y, z) | x <- [1..n], y <- [x+1..n]
                       , coprime x y
                       , z <- [y+1..n], x^2 + y^2 == z^2]
-- |
-- 指定した範囲にあるピタゴラス数の列挙
triads :: Int -> [(Int, Int, Int)]
triads n = [(x, y, z) | x <- [1..m], y <- [x+1..n]
                       , coprime x y
                       , z <- [y+1..n], x^2 + y^2 == z^2]
           where
             m = floor (fromIntegral n / sqrt 2)

-- |
-- 約数の列挙
divisors :: Integral a => a -> [a]
divisors x = [ d | d <- [2 .. x-1], x`mod` d == 0]

-- |
-- 互いに素（整数）
coprime :: Integral a => a -> a -> Bool
coprime x y = disjoint (divisors x) (divisors y)

-- |
-- 互いに素（集合）
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint xxs@(x:xs) yys@(y:ys) = case compare x y of
  LT -> disjoint xs yys
  EQ -> False
  GT -> disjoint xxs ys

-- |
-- 互いに素（整数）の別版
coprime' :: Integral a => a -> a -> Bool
coprime' = ((1 ==) .) . gcd

{- $TranslationOfListComprehension
リスト内包表記の翻訳規則
@
[e | True]       => [e]
[e | q]          => [e | q, True]
[e | b, Q]       => if b then [e | Q] else []
[e | p <- xs, Q] => let ok p = [e | Q]
                        ok _ = []
                    in concat (map ok xs)
[e | Q1, Q2]     => concat [[e | Q2] | Q1]
@
-}

-- |
-- nullの定義例
-- @
-- null []    = True 
-- null (_:_) = False
-- @
null :: [a] -> Bool
null []    = True 
null (_:_) = False

-- |
-- headの定義例
-- @
-- head (x:_) = x
-- @
head :: [a] -> a
head (x:_) = x

-- |
-- tailの定義例
-- @
-- tail (_:xs) = xs
-- @
tail :: [a] -> [a]
tail (_:xs) = xs

-- |
-- lastの定義例
-- @
-- last [x]          = x
-- last (_:ys@(_:_)) = last ys
-- @
last :: [a] -> a
last [x]          = x
last (_:ys@(_:_)) = last ys

-- |
-- (++)の定義例
-- @
-- []       ++ ys = ys            
-- (x : xs) ++ ys = x : (xs ++ ys)
-- @
(++) :: [a] -> [a] -> [a]
[]       ++ ys = ys            
(x : xs) ++ ys = x : (xs ++ ys)

-- |
-- lengthの定義例
-- @
-- length []       = 0
-- length (_ : xs) = 1 + length xs
-- @
length :: [a] -> Int
length []       = 0
length (_ : xs) = 1 + length xs

-- |
-- concatの定義例
-- @
-- concat []         = []
-- concat (xs : xss) = xs ++ concat xss
-- @
concat :: [[a]] -> [a]
concat []         = []
concat (xs : xss) = xs ++ concat xss

-- |
-- mapの定義例
-- @
-- map _ []       = []
-- map f (x : xs) = f x : map f xs
-- @
map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (x : xs) = f x : map f xs

-- |
-- filterの定義例
-- @
-- filter _ []       =  []
-- filter p (x : xs) = if p x then x : filter p xs else filter p xs
-- @
filter :: (a -> Bool) -> [a] -> [a]
filter _ []       =  []
filter p (x : xs) = if p x then x : filter p xs else filter p xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = concat . map (test p)

test :: (a -> Bool) -> a -> [a]
test p x = if p x then [x] else []

{-
Data.Maybe.mapMaybe を使って以下のように定義することも可能

test' :: (a -> Bool) -> a -> Maybe a
test' p x = bool Nothing (Just x) (p x)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' = mapMaybe . test'
-}

-- |
-- zipの定義例
-- @
-- zip (x:xs) (y:ys) = (x,y) : zip xs ys
-- zip _      _      = []
--
-- zip' [] ys = []
-- zip' (x:xs) [] = []
-- zip' (x:xs) (y:ys) = (x,y):zip xs ys
--
-- zip'' = zipWith (,)
-- @
zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _      _      = []

zip' :: [a] -> [b] -> [(a,b)]
zip' [] ys = []
zip' (x:xs) [] = []
zip' (x:xs) (y:ys) = (x,y):zip xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith (,)
-- |
-- zipWithの定義例
-- @
-- zipWith f (x:xs) (y:ys) = f x y : zipWith xs ys
-- zipWith _ _      _      = []
-- @
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _      _      = []

-- |
-- 非減少列判定
-- @
-- nondec [] = True
-- nondec [x] = True
-- nondec (x:y:zs) = x <= y && nondec (y:zs)
--
-- nondec' :: (Ord a) => [a] -> Bool
-- nondec' [] = True
-- nondec' xs = and (zipWith (<=) xs xs')
-- @
nondec :: (Ord a) => [a] -> Bool
nondec [] = True
nondec [x] = True
nondec (x:y:zs) = x <= y && nondec (y:zs)

nondec' :: (Ord a) => [a] -> Bool
nondec' [] = True
nondec' xs = and (zipWith (<=) xs (tail xs))

-- |
-- 最初の出現位置特定
-- @
-- commonWords :: Int -> Text -> String
-- commonWords n = concat . map showRun . take n
--               . sortRuns . countRuns . sortWords
--               . words . map toLower

-- type Text = String
-- type Word = String

-- showRun :: (Int,Word) -> String
-- showRun (n,w) = w ++ ": " ++ show n ++ "\n"

-- countRuns :: [Word] -> [(Int, Word)]
-- countRuns []     = []
-- countRuns (w:ws) = (1 + length us, w) : countRuns vs
--                    where
--                      (us,vs) = span (== w) ws

-- sortWords :: [Word] -> [Word]
-- sortWords = sort

-- sortRuns :: [(Int,Word)] -> [(Int,Word)]
-- sortRuns = reverse . sort

-- sortRuns':: [(Int,Word)] -> [(Int,Word)]
-- sortRuns' = sortBy (flip (comparing fst))
-- @
position :: (Eq a) => a -> [a] -> Int
position x xs = head ([j | (j, y) <- zip [0..] xs, y == x] ++ [-1])

-- |
-- 頻出単語
-- @
-- @
commonWords :: Int -> Text -> String
commonWords n = concat . map showRun . take n
              . sortRuns . countRuns . sortWords
              . words . map toLower

type Text = String
type Word = String

showRun :: (Int,Word) -> String
showRun (n,w) = w ++ ": " ++ show n ++ "\n"

countRuns :: [Word] -> [(Int, Word)]
countRuns []     = []
countRuns (w:ws) = (1 + length us, w) : countRuns vs
                   where
                     (us,vs) = span (== w) ws

sortWords :: [Word] -> [Word]
sortWords = sort

sortRuns :: [(Int,Word)] -> [(Int,Word)]
sortRuns = reverse . sort

sortRuns':: [(Int,Word)] -> [(Int,Word)]
sortRuns' = sortBy (flip (comparing fst))

-- |
-- ソート
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
          where
            (ys,zs) = halve xs

halve :: [a] -> ([a],[a])
halve xs = splitAt n xs
  where
    n = length xs `div` 2

-- |
-- マージソート
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs@(_:_:_) = mergeAll (divide xs)

divide [] = []
divide [x] = [[x]]
divide (x:y:zs) | x <= y    = [x,y]:divide zs
                | otherwise = [y,x]:divide zs

mergeAll [xs] = xs
mergeAll xss  = mergeAll (mergePairs xss)

mergePairs (xs:ys:zss) = merge xs ys : mergePairs zss
mergePairs xss         = xss

merge xxs@(x:xs) yys@(y:ys)
  | x <= y    = x : merge xs yys
  | otherwise = y : merge xxs ys
merge [] ys = ys
merge xs [] = xs

