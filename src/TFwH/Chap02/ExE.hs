{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- = 第2章 練習問題 E
--
module TFwH.Chap02.ExE where

import Control.Exception
import Data.List

-- $setup

-- |
--
-- @
-- first :: (a -> Bool) -> [a] -> a
-- first p = head . filter p
-- @
--
-- @
-- first :: (a -> Bool) -> [a] -> a
-- first p xs | null xs   = error "Empty list"
--            | p x       = x
--            | otherwise = firstB p (tail xs)
--            where
--              x = head xs
-- @
--
-- >>> let firstS p = head . filter p  -- Susan版
-- >>> first isSquare nums == firstS isSquare nums
-- True
first :: (a -> Bool) -> [a] -> a
first p xs | null xs   = error "Empty list"
           | p x       = x
           | otherwise = first p (tail xs)
           where
             x = head xs
-- |
-- Susan の first' :: (b -> Bool) -> (a -> b) -> [a] -> b
--
-- @
-- first' :: (b -> Bool) -> (a -> b) -> [a] -> b
-- first' p f = head . filter p . map f
-- @
--
-- Beaver の first'
--
-- @
-- first'  :: (b -> Bool) -> (a -> b) -> [a] -> b
-- first' p f xs | null xs = error "Empty list"
--               | p x     = x
--               | otherwise = first' p f (tail xs)
--               where
--                 x = f (head xs)
-- @
--
-- >>> let first'S p f = head . filter p . map f
-- >>> first' isSquare (subtract 1 . (2*)) nums == first'S isSquare (subtract 1 . (2*)) nums
-- True
first'  :: (b -> Bool) -> (a -> b) -> [a] -> b
first' p f xs | null xs = error "Empty list"
              | p x     = x
              | otherwise = first' p f (tail xs)
              where
                x = f (head xs)
                
-- |
-- より好ましい betterFistB (Beaver 版)
--
-- @
-- betterFirstB :: (a -> Bool) -> [a] -> Maybe a
-- betterFirstB p xs
--   | null xs   = Nothing
--   | p x       = Just x
--   | otherwise = betterFirstBeaver p (tail xs)
--   where
--     x = head xs
-- @
--
betterFirstB :: (a -> Bool) -> [a] -> Maybe a
betterFirstB p xs = case uncons xs of
  Nothing     -> Nothing
  Just (y,ys) -> if p y then Just y else betterFirstB p ys

-- |
-- より好ましい betterFistS (Susan 版)
--
-- @
-- betterFirstS :: (a -> Bool) -> [a] -> Maybe a
-- betterFirstS p = maybe Nothing (Just . fst) . uncons . filter p
-- @
--
-- >>> sum nums'
-- 499999500000
-- >>> betterFirstB isSquare nums' == betterFirstS isSquare nums
-- True

betterFirstS :: (a -> Bool) -> [a] -> Maybe a
betterFirstS p = maybe Nothing (Just . fst) . uncons . filter p

nums :: [Int]
nums =  [999999, 999998 .. 0]

nums' :: [Int]
nums' = reverse [0 .. 999999]

isSquare :: Int -> Bool
isSquare = (==) <*> (^2) . round . sqrt . fromIntegral 

{- | 多相関数 :: Maybe a -> Maybe a
>>> import Control.Exception
>>> 
-}
poly :: Int -> Maybe a -> Maybe a
poly 0 x = case x of
  Nothing -> Nothing
  Just y  -> Nothing
poly 1 x = case x of
  Nothing -> Nothing
  Just y  -> Just y
poly 2 x = case x of
  Nothing -> Nothing
  Just y  -> Just undefined
poly 3 x = case x of
  Nothing -> Nothing
  Just y  -> undefined
poly 4 x = case x of
  Nothing -> Just undefined
  Just y  -> Nothing
poly 5 x = case x of
  Nothing -> Just undefined
  Just y  -> Just y
poly 6 x = case x of
  Nothing -> Just undefined
  Just y  -> Just undefined
poly 7 x = case x of
  Nothing -> Just undefined
  Just y  -> undefined
poly 8 x = case x of
  Nothing -> undefined
  Just y  -> Nothing
poly 9 x = case x of
  Nothing -> undefined
  Just y  -> Just y
poly 10 x = case x of
  Nothing -> undefined
  Just y  -> Just undefined
poly 11 x = case x of
  Nothing -> undefined
  Just y  -> undefined

poly 12 x = Nothing
poly 13 x = Just undefined
poly 14 x = Just $ case x of
                     Nothing -> undefined
                     Just y  -> y
  
poly 15 x = case x of
  Nothing -> Nothing
  Just !y -> Just y
poly 16 x = case x of
  Nothing -> Just undefined
  Just !y -> Just y
poly 17 x = case x of
  Nothing -> undefined
  Just !y -> Just y

prna :: Show a => a -> IO String
prna a = catch (return $ show a) (\ (e :: SomeException) -> return "undefined")
         
prn :: Show a => Maybe a -> IO ()
prn m = catch (case m of
                 Nothing -> putStrLn "Nothing"
                 Just x  -> putStrLn . ("Just "++) =<< prna x
              ) (\ (e :: SomeException) -> putStrLn "undefined")

samples = [Nothing, Just (), Just undefined, undefined] :: [Maybe ()]

ps :: Show a => [Maybe a -> IO ()]
ps = map ((prn .) . poly) [0..17]

test = mapM_ sequence_ $ intersperse [putStrLn "-----"] $ map (flip map samples) ps
