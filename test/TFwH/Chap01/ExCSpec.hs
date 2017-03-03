{-# LANGUAGE ExtendedDefaultRules #-}
module TFwH.Chap01.ExCSpec where

import TFwH.Chap01.ExC
import Test.Hspec
import Data.Typeable

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "第1章練習問題C" $ do
     { context "'H' の型は" $ do
         { it "Char のはず．" $ do
             { show (typeOf 'H') `shouldBe` "Char"
             }
         }
     ; context "\"H\" の型は" $ do
         { it "\"[Char]\" のはず．"  $ do
             { show (typeOf "H") `shouldBe` "[Char]"
             }
         }
     ; context "2001 の型は" $ do
         { it "Integer のはず．"  $ do
             { show (typeOf 2001) `shouldBe` "Integer"
             }
         }
     ; context "\"2001\" の型は" $ do
         { it "[Char] のはず．"  $ do
             { show (typeOf "2001") `shouldBe` "[Char]"
             }
         }
     ; context "[1, 2, 3] ++ [3, 2, 1] は" $ do
         { it "[1,2,3,3,2,1] のはず．" $ do
             { [1, 2, 3] ++ [3, 2, 1] `shouldBe` [1,2,3,3,2,1]
             }
         }
     ; context "\"Hello\" ++ \" World!\" は" $ do
         { it "\"Hello World!\" のはず．" $ do
             { "Hello" ++ " World!" `shouldBe` "Hello World!"
             }
         }
     ; context "[1, 2, 3] ++ [] は" $ do
         { it "[1,2,3] のはず．" $ do
             { [1, 2, 3] ++ [] `shouldBe` [1,2,3]
             }
         }
     ; context "\"Hello\" ++ \" \" ++ \"Hello!\" は" $ do
         { it "\"Hello World!\" のはず．" $ do
             { "Hello" ++ " " ++ "World!" `shouldBe` "Hello World!"
             }
         }
     }
