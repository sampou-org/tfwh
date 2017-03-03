module TFwH.Chap01.ExBSpec (main, spec) where

import TFwH.Chap01.ExB
import Test.Hspec
import Data.Typeable

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "第1章練習問題B" $ do
     { context "sin theta^2 の型は" $ do
         { it "theta の型と同じはず．" $ do
             { typeOf (sin theta^2) == typeOf theta `shouldBe` True
             }
         }
     ; context "(sin theta)^2 の型は" $ do
         { it "theta の型と同じはず．"  $ do
             { typeOf ((sin theta )^2) == typeOf theta `shouldBe` True
             }
         }
     }
