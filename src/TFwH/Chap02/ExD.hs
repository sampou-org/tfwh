-- |
-- = 第2章 練習問題 D
--
module TFwH.Chap02.ExD where
-- $setup
-- >>> isSquare n = (n ==) $ (^2) $ round $ sqrt $ fromIntegral n
-- >>> nums :: [Int]; nums = [2..1000000]
-- >>> nums == nums
-- True

-- |
-- Susan版 first は
--
-- @
-- first :: (a -> Bool) -> [a] -> a
-- first p = head . filter p
-- @
--
-- Beaver版 first は
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
