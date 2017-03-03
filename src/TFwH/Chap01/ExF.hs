module TFwH.Chap01.ExF where

import Prelude hiding (Word)
import Data.Char (toLower)
import Data.List (sortBy)
import Data.Ord (comparing)

type Word = String
type Label = String
type Labeled = (,) Label
type Anagram = (Label, [Word])

-- |
-- anagrams n はアルファベット順の英単語リストを取り，n文字の単語だけをとりだし，文字列を生成する．
-- 結果の文字列を表示するとn文字の単語のアナグラムの一覧となるものとする．
--
anagrams :: Int -> [Word] -> String
anagrams n = unlines           -- ^ 一覧表示
           . map showAnagram   -- ^ アナグラムを表示
           . groupByLabel      -- ^ 同一ラベルの単語をグループ化
           . sortByLabel       -- ^ アナグラムラベルでソート
           . map addLabel      -- ^ アナグラムラベルを単語に追加
           . selectByLength n  -- ^ n文字の単語だけ選択

selectByLength :: Int -> [Word] -> [Word]
selectByLength n = undefined

addLabel :: Word -> Labeled Word
addLabel w = undefined

sortByLabel :: [Labeled Word] -> [Labeled Word]
sortByLabel = undefined

groupByLabel :: [Labeled Word] -> [Anagram]
groupByLabel = undefined

showAnagram :: Anagram -> String
showAnagram = undefined
