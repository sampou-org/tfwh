-- |
-- = 第2章 練習問題 C
--
--    * 関数 toLower :: Char -> Char は英字を小文字に変換する．
--      標準ライブラリの英字を大文字に変換する関数はどういう名前だと思うか．
--
--        * toUpper
--
--    * 関数 words :: String -> [Word] は前章で使った関数である．
--      プレリュード関数 unwords :: [Word] -> String は何をする関数だと思うか．
--      ヒント：以下の等式の一方だけが成り立つとすればどちらか．
--
--        @
--        words . unwords = id
--        unwords . words = id
--        @
--
--        * unwords . words = id
--
--    * 関数 head :: [a] -> a は空ではないリストの先頭を返す．
--      関数 tail :: [a] -> [a] は空ではないリストから先頭要素を取り除いた残りのリストすなわち末尾部分を返す．
--      あるリストの先頭が x で末尾部が xs だとすると，もとのリストを再構成するにはどうすればよいか．
--
--        * [x] ++ xs
--
module TFwH.Chap02.ExC where

import Data.Char (toUpper)
import Prelude hiding (Word)

type Word = String

-- |
--
-- @
-- capitaliseTitle = unwords . map capitalize . words
-- @
--
-- >>> modernise "The morphology of prex -- an essay in meta-algorithmics"
-- "The Morphology Of Prex -- An Essay In Meta-Algorithmics"
--
modernise :: String -> String
modernise = unwords . map capitalise . words

-- |
-- 本文の解答では
--
-- @
-- capitalise xs = [toUpper (head xs)] ++ tail xs
-- @
--
-- となっているが，これだと capitalise "meta-algorithmics" ≠ "Meta-Algorithmics" である．
-- capitalise "meta-algorithmics" ＝ "Meta-Algorithmics" であるためには以下のような実装でなければならい．
-- 
-- @
-- capitalise cs = case break ('-'==) cs of
--   (x:xs,"")   -> toUpper x : xs
--   (x:xs,y:ys) -> toUpper x : (xs ++ y : capitalise ys)
--   _           -> cs
-- @
--
-- >>> capitalise "the"
-- "The"
-- >>> capitalise "meta-algorithmics"
-- "Meta-Algorithmics"
--
capitalise cs = case break ('-'==) cs of
  (x:xs,"")   -> toUpper x : xs
  (x:xs,y:ys) -> toUpper x : (xs ++ y : capitalise ys)
  _           -> cs

