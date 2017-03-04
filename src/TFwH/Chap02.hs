-- |
--
-- TFwH 第2章 式,型,値
--
module TFwH.Chap02
  ( -- * 2 式,型,値
    -- ** 2.1 GHCiを使ったセッション
    -- ** 2.2 名前と演算子
    (+++)
    -- ** 2.3 評価
  , sqr
  , infinity
  , three
  , factorial
  , fact
  , to
    -- ** 型と型クラス
  , Person(..)
  , samePerson
    -- ** 値の表示
  , cwords
    -- ** Haskellのレイアウト
  , roots
  ) where

import TFwH.CommonWords

-- |
-- ユーザ定義演算子
--
(+++) :: Int -> Int -> Int
x +++ y = if even x then y else x + y

-- |
-- 整数の2乗
--
sqr :: Integer -> Integer
sqr x = x * x

-- |
-- 無限大
--
infinity :: Integer
infinity = 1 + infinity

-- |
-- 常に3になる定数関数
--
three :: Integer -> Integer
three x = 3

-- |
-- 階乗関数
--
factorial :: Integer -> Integer
factorial n = fact (n, 1)

-- |
-- 下請け関数
--
fact :: (Integer, Integer) -> Integer
fact (x, y) = if x == 0 then y else fact (x - 1, x * y)

-- |
-- 無限ループする関数
--
to :: Bool -> Bool
to b = not (to b)

-- |
-- Person(例)
--
data Person = Person { pin :: Pin }
type Pin = Int

instance Eq Person where
  x == y = pin x == pin y

samePerson :: Person -> Person -> Bool
samePerson x y = pin x == pin y

-- |
-- 頻出単語
--
cwords :: Int -> FilePath -> FilePath -> IO ()
cwords n infile outfile
  = do { text <- readFile infile
       ; writeFile outfile (commonWords n text)
       ; putStrLn "cwords done!"
       }

main :: IO ()
main = do { putStrLn "Take text from where:"
          ; infile <- getLine
          ; putStrLn "How many words:"
          ; n <- getLine
          ; putStrLn "Put results where:"
          ; outfile <- getLine
          ; text <- readFile infile
          ; writeFile outfile (commonWords (read n) text)
          ; putStrLn "cwords done!"
          }

-- |
-- 2次方程式の解
--
roots :: (Float, Float, Float) -> (Float, Float)
roots (a, b, c)
  | a == 0 = error "not quadratic"
  | disc < 0 = error "complex roots"
  | otherwise = ((-b - r) / e, (-b + r) / e)
  where
    { disc = b * b - 4 * a * c; r = sqrt disc; e = 2 * a }
