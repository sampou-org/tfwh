-- |
--
-- TFwH 第1章 関数プログラミングとは何か
--
module TFwH.Chap01
  ( -- * 1 関数プログラミングとは何か
    -- ** 1.1 関数と型
    -- ** 1.2 関数合成
    -- ** 1.3 頻出単語
    Text
  , Word
  , commonWords
  , sortWords
  , countRuns
  , sortRuns
  , showRun
    -- ** 1.4 例題：数を言葉にする
  , convert
  , units
  , teens
  , tens
  , convert1
  , digits2
  , convert2
  , combine2
  , convert3
  , convert6
  , link
  ) where

import Data.Char (toLower)
import Prelude hiding (Word)

-- 1.1 関数と型
-- 1.2 関数合成
-- 1.3 頻出単語
-- |
-- テキストの型
--
type Text = [Char]
-- |
-- 単語の型
--
type Word = [Char]

-- |
-- 
-- commonWords n はテキストから頻出単語の出現回数の上位 n 個の単語を出現回数とともに表示するための文字列を生成する．
--
-- @
-- commonWords n = concat       -- 連結する
--               . map showRun  -- 単語と出現回数をを表示する文字列を生成する
--               . take n       -- 上位 n 個を取り出す
--               . sortRuns     -- 出現回数順でソートする
--               . countRuns    -- 出現回数を数えて単語と出現回数を対にする
--               . sortWords    -- 単語をソートする
--               . words        -- テキストを単語に分解
--               . map toLower  -- テキストの文字をすべて小文字にする
-- @
--
commonWords :: Int -> Text -> String
commonWords n = concat
              . map showRun
              . take n
              . sortRuns
              . countRuns
              . sortWords
              . words
              . map toLower

-- |
-- 未定義
--
sortWords :: [Word] -> [Word]
sortWords = undefined

-- |
-- 未定義
--
countRuns :: [Word] -> [(Int, Word)]
countRuns = undefined

-- |
-- 未定義
--
sortRuns :: [(Int, Word)] -> [(Int, Word)]
sortRuns = undefined

-- |
-- 未定義
--
showRun :: (Int, Word) -> String
showRun = undefined

-- 1.4 例題：数を言葉に変換する
-- |
-- 1桁の数
--
units :: [String]
units = [ "zero", "one", "two", "three", "four", "five"
        , "six", "seven", "eight", "nine"
        ]

-- |
-- 10台の数
--
teens :: [String]
teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen"
        , "fifteen", "sixteen", "seventeen", "eighteen"
        , "nineteen"
        ]

-- |
-- 10の倍数
--
tens :: [String]
tens  = [ "twenty", "thirty", "forty", "fifty", "sixty"
        , "seventy", "eighty", "ninety"
        ]

-- |
-- 1桁の数の変換
--
convert1 :: Int -> String
convert1 n = units !! n

-- |
-- 2桁の数の桁分解
--
digits2 :: Int -> (Int, Int)
digits2 n = n `divMod` 10

-- |
-- 2桁の数の変換
--
convert2 :: Int -> String
convert2 = combine2 . digits2

-- |
-- 2つの桁を合成
--
combine2 :: (Int, Int) -> String
combine2 (t, u)
  | t == 0           = units !! u
  | t == 1           = teens !! u
  | 2 <= t && u == 0 = tens !! (t - 2)
  | 2 <= t && u /= 0 = tens !! (t - 2) ++ "-" ++ units !! u

-- |
-- 3桁の数の変換
--
convert3 :: Int -> String
convert3 n
  | h == 0    = convert2 t
  | t == 0    = units !! h ++ " hundred"
  | otherwise = units !! h ++ " hundreds and " ++ convert2 t
  where
    (h, t) = n `divMod` 100

-- |
-- 6桁の数の変換
--
convert6 :: Int -> String
convert6 n
  | m == 0    = convert3 h
  | h == 0    = convert3 m ++ " thousand"
  | otherwise = convert3 m ++ " thousand" ++ link h ++ convert3 h
  where
    (m, h) = n `divMod` 1000

-- |
-- 接続詞
--
link :: Int -> String
link h = if h < 100 then " and " else " "

-- |
-- 6桁以下の数を英単語に変換する
--
-- >>> convert 308000
-- "three hundreds and eight thousand"
-- >>> convert 369027
-- "three hundreds and sixty-nine thousand and twenty-seven"
-- >>> convert 369401
-- "three hundreds and sixty-nine thousand four hundreds and one"
--
convert :: Int -> String
convert = convert6

