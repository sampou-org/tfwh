-- |
-- = 第2章 練習問題 I
-- 
--    * 入力文字列が回文になっているかを判定する対話プログラム@palindrome :: IO ()@を書け
--
--        @
--        import Data.Char
--
--        parindrome :: IO ()
--        prindrome
--          = do { putStrLn "Enter a string:"
--               ; xs <- getLine
--               ; if isPalindrome xs then putStrLn "Yes!"
--                 else putStrLn "No!"
--               }
--
--        isPalindrome :: String -> Bool
--        isPalindrome xs = (ys == reverse ys)
--                          where
--                            ys = map toLower (filter isAlpha xs)
--        @
--
module TFwH.Chap02.ExI where

import Data.Char

-- |
-- 入力文字列が回文になっているかを判定する対話プログラム
--
palindrome :: IO ()
palindrome
  = do { putStrLn "Enter a string:"
       ; xs <- getLine
       ; if isPalindrome xs then putStrLn "Yes!"
         else putStrLn "No!"
       }

-- |
-- 文字列が回文かどうかを判定する述語
--
-- >>> isPalindrome "Madam, I'm Adam"
-- True
-- >>> isPalindrome "A Man, a plan, a canal - Suez!"
-- False
-- >>> isPalindrome "A Man, a plan, a canal - Panama"
-- True
--
isPalindrome :: String -> Bool
isPalindrome xs = (ys == reverse ys)
                  where
                    ys = map toLower (filter isAlpha xs)
