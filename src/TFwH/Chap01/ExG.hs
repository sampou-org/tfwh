module TFwH.Chap01.ExG where

song :: Int -> String
song n = if n == 0 then ""
         else song (n - 1) ++ "\n" ++ verse n

verse :: Int -> String
verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

line1 :: Int -> String
line1 n = if n == 1 then "One man went to mow\n"
          else numbers !! (n - 2) ++ " men went to mow\n"

line2 :: Int -> String
line2 n = "Went to mow a meadow\n"

line3 :: Int -> String
line3 n = if n == 1 then "One man add his dog\n"
          else numbers !! (n - 2) ++ " men " ++ count (n - 2) ++ "one man and his dog\n"

line4 :: Int -> String
line4 n = "Wen to mow a meadow\n"

count :: Int -> String
count n = if n == 0 then ""
          else numbs !! (n - 1) ++ " men, " ++ count (n - 1)

numbers, numbs :: [String]
numbers = ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
numbs   = ["two", "three", "four", "five", "six", "seven", "eight"]
