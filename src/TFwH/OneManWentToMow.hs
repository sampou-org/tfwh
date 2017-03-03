-- |
-- One man went to mow
--
module TFwH.OneManWentToMow (song) where

-- |
-- song n は1人からn人まで登場する歌詞を表示する文字列を生成する
--
-- >>> let shownl c = if c /= '\n' then [c] else "\\n\n"
-- >>> let shownls = concatMap shownl
-- >>> putStr $ shownls $ song 3
-- \n
-- One man went to mow\n
-- Went to mow a meadow\n
-- One man add his dog\n
-- Went to mow a meadow\n
-- \n
-- Two men went to mow\n
-- Went to mow a meadow\n
-- Two men, one man and his dog\n
-- Went to mow a meadow\n
-- \n
-- Three men went to mow\n
-- Went to mow a meadow\n
-- Three men, two men, one man and his dog\n
-- Went to mow a meadow\n
--
song :: Int -> String
song n = if n == 0 then ""
         else song (n - 1) ++ "\n" ++ verse n

verse :: Int -> String
verse n = unlines $ map ($ n) [line1, line2, line3, line4]

line1 :: Int -> String
line1 n = if n == 1 then "One man went to mow"
          else numbers !! (n - 2) ++ " men went to mow"

line2 :: Int -> String
line2 n = "Went to mow a meadow"

line3 :: Int -> String
line3 n = if n == 1 then "One man add his dog"
          else numbers !! (n - 2) ++ " men, " ++ count (n - 2) ++ "one man and his dog"

line4 :: Int -> String
line4 n = "Went to mow a meadow"

count :: Int -> String
count n = if n == 0 then ""
          else numbs !! (n - 1) ++ " men, " ++ count (n - 1)

numbers, numbs :: [String]
numbers = ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
numbs   = ["two", "three", "four", "five", "six", "seven", "eight"]
