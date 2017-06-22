-- |
-- = 第2章 練習問題 H
-- 
--    * @addSum :: CIN -> CIN@ を構成せよ
--
--        @
--        type CIN = String
--
--        addSum :: CIN -> CIN
--        addSum cin = cin ++ show (n `div` 10) ++ show (n `mod` 10)
--                     where
--                       n = sum (map getDigit cin)
--
--        getDigit :: Char -> Int
--        getDigit c = read [c]
--        @
--
--    * 識別番号が正しいかどうかを判定する関数 @valid :: CIN -> Bool@ を構成せよ
--
--        @
--        valid :: CIN -> Bool
--        valid cin = cin == addSum (take 8 cin)
--        @
--
module TFwH.Chap02.ExH where

import Data.Char

type CIN = String

-- |
-- 8桁のIDにチェックサムを追加して10桁のIDにする
-- @getDigit@ を定義せずに，@Data.Char.digitToInt :: Char -> Int@ を使いました．
-- 剰余計算は@div@と@mod@を別々に使わず，@divMod@ を使いました．
--
-- @
-- addSum :: CIN -> CIN
-- addSum cin = cin ++ show q ++ show r
--              where
--                (q,r) = sum (map digitToInt cin) `divMod` 10
-- @
--
-- >>> addSum "12345678"
-- "1234567836"
--
addSum :: CIN -> CIN
addSum cin = cin ++ show q ++ show r
             where
               (q,r) = sum (map digitToInt cin) `divMod` 10

-- |
-- 識別番号が正しいかどうかを判定する
--
-- @Data.Char@ をインポートしてより細かいチェックを実施
--
-- @
-- valid :: CIN -> Bool
-- valid cin = length cin == 10
--          && all isDigit cin
--          && cin == addSum (take 8 cin)
-- @
--
-- >>> valid "123456"
-- False
-- >>> valid "abcd56789a4"
-- False
-- >>> valid "1234567890"
-- False
-- >>> valid "12345678036"
-- False
-- >>> valid "1234567836"
-- True
--
valid :: CIN -> Bool
valid cin = length cin == 10
         && all isDigit cin
         && cin == addSum (take 8 cin)
         
