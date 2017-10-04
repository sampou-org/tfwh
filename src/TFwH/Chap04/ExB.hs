-- |
-- = 第4章 練習問題 B
--
--     * 自然数の対で互いに相異なるものすべての無限リストを生成したいとする．
--       すべての対が現れる限りにおいて，対の列挙順は重要ではない．
--       以下の定義は正しく働くといえるか．
--
--       @
--       allPairs = [(x, y) | x <- [0..], y <- [0..]]
--       @
--
--       これが正しく働かないと思うなら，正しく働く版を示せ．
--
module TFwH.Chap04.ExB where

import Numeric.Natural

allPairs :: [(Natural, Natural)]
allPairs = [ (n,s-n) | s <- [0..], n <- [0..s]]

