-- |
--
-- 頻出単語
--
module TFwH.CommonWords where

import Data.Char (toLower)
import Data.List (sort,sortBy,group)
import Data.Ord (comparing)
import Prelude hiding (Word)

-- |
-- テキストの型
--
type Text = [Char]
-- |
-- 単語の型
--
type Word = [Char]

-- |
-- commonWords n はテキストから頻出単語の出現回数の上位 n 個の単語を出現回数とともに表示するための文字列を生成する．
--
-- @
-- commonWords n = unlines      -- 連結する
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
commonWords n = unlines
              . map showRun
              . take n
              . sortRuns
              . countRuns
              . sortWords
              . words
              . map toLower

-- |
-- 単語をソートする
--
sortWords :: [Word] -> [Word]
sortWords = sort

-- |
-- 出現回数を数えて単語と出現回数を対にする
--
countRuns :: [Word] -> [(Int, Word)]
countRuns = map addCount . group
  where
    addCount ws = (length ws, head ws)

-- |
-- 出現回数順でソートする
--
sortRuns :: [(Int, Word)] -> [(Int, Word)]
sortRuns = sortBy (flip (comparing fst))

-- |
-- 単語と出現回数をを表示する文字列を生成する
--
showRun :: (Int, Word) -> String
showRun (n, w) = w ++ ": " ++ show n

