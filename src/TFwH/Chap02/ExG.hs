{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- = 第2章 練習問題 G
-- 
--    * @showDate :: Date -> String@ を定義せよ
--
--        @
--        type Year  = Int
--        type Month = Int
--        type Day   = Int
--
--        type Date  = (Day, Month, Year)
--
--        showDate :: Date -> String
--        showDate (d, m, y) = show d ++ suffix d ++ " "
--                          ++ months !! (m - 1) ++ ", " ++ show y
--
--        months = [ "January", "Feburary", "March", "April"
--                 , "May", "June", "July", "August"
--                 , "September", "October", "November", "December"
--                 ]
--        @
--
module TFwH.Chap02.ExG where

type Year  = Int
type Month = Int
type Day   = Int

type Date = (Day, Month, Year)


-- |
-- 日付を表示文字列に変換
--
-- >>> showDate (10, 12, 2013)
-- "10th December, 2013"
-- >>> showDate (21, 11, 2020)
-- "21st November, 2020"
--
showDate :: Date -> String
showDate (d, m, y) = show d ++ suffix d ++ " "
                  ++ months !! (m - 1) ++ ", " ++ show y

-- |
-- 日付序数詞の接尾辞
--
suffix :: Day -> String
suffix d = if d == 1 || d == 21 || d == 31 then "st"
           else if d == 2 || d == 22 then "nd"
                else if d == 3 || d == 23 then "rd"
                     else "th"

-- |
-- 月の名
--
months = [ "January", "Feburary", "March", "April"
         , "May", "June", "July", "August"
         , "September", "October", "November", "December"
         ]
