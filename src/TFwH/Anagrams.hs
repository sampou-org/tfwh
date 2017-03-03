-- |
--
-- アナグラム一覧
--
module TFwH.Anagrams where

import Prelude hiding (Word)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (sort, sortBy, groupBy, intercalate)
import Data.Ord (comparing)

-- |
-- 単語の型
--
type Word = String

-- |
-- ラベルの型
--
type Label = String

-- |
-- anagrams n はアルファベット順の英単語リストから n 文字の単語だけを選別し,
-- 同じアナグラムになる単語をまとめた結果を表示するための文字列を生成する．
-- この文字列を表示するとn文字の単語のアナグラムの一覧となる．
--
-- @
-- anagrams n = unlines        -- 連結する
--            . map showEntry  -- エントリーを文字列に変換する
--            . groupByLabel   -- 同一ラベルのラベル付き単語をグループ化してエントリー作成する
--            . sortLabels     -- ラベル付き単語をラベルの辞書順でソートする
--            . map addLabel   -- 単語にラベルを追加する
--            . getWords n     -- n 文字の単語だけ取り出す
-- @
--
anagrams :: Int -> [Word] -> String
anagrams n = unlines
           . map showEntry
           . groupByLabel
           . sortLabels
           . map addLabel
           . getWords n

-- |
-- n 文字の単語だけを濾過
--
getWords :: Int -> [Word] -> [Word]
getWords n = filter ((n ==) . length)

-- |
-- 単語にラベルを追加
--
addLabel :: Word -> (Label, Word)
addLabel w = (sort (map toLower w), w)

-- |
-- ラベル付き単語をラベルの辞書順でソート
--
sortLabels :: [(Label, Word)] -> [(Label, Word)]
sortLabels = sortBy (comparing fst)

-- |
-- 同一ラベルの付いた単語をグループ化してエントリー作成
--
groupByLabel :: [(Label, Word)] -> [(Label, [Word])]
groupByLabel = map mkEntry . groupBy ((==) `on` fst)
  where
    mkEntry lws = (head ls, ws)
      where
        (ls,ws) = unzip lws

-- |
-- エントリーを文字列に変換
--
showEntry :: (Label, [Word]) -> String
showEntry (l, ws)= l ++ ": " ++ intercalate "," ws

