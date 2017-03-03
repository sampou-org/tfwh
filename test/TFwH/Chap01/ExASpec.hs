module TFwH.Chap01.ExASpec (main, spec) where

import TFwH.Chap01.ExA

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "第1章練習問題A" $ do
     { context "map double [1, 4, 4, 3] は" $ do
         { it "[2,8,8,6] のはず．" $ do
             { map double [1, 4, 4, 3] `shouldBe` [2,8,8,6]
             }
         }
     ; context "map (double . double) [1, 4, 4, 3] は" $ do
         { it "[4,16,16,12] のはず．"  $ do
             { map (double . double) [1, 4, 4, 3] `shouldBe` [4,16,16,12]
             }
         }
     ; context "map double [] は" $ do
         { it "[] のはず．" $ do
             { map double [] `shouldBe` []
             }
         }
     }
