{-# LANGUAGE ExtendedDefaultRules #-}
module TFwH.Chap01.ExDSpec where

import Data.Char
import TFwH.Chap01.ExD
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

sample :: String
sample = "To be or not to be"

spec :: Spec
spec = describe "第1章練習問題D" $ do
     { context "words . map toLower $ \"To be or not to be\" は"  $ do
         { it "[\"to\",\"be\",\"or\",\"not\",\"to\",\"be\"] のはず．"  $ do
             { (words . map toLower) "To be or not to be" `shouldBe` ["to","be","or","not","to","be"]
             }
         }
     ; context "map (map toLower) . words $ \"To be or not to be\" は"  $ do
         { it "[\"to\",\"be\",\"or\",\"not\",\"to\",\"be\"] のはず．"  $ do
             { (map (map toLower) . words) "To be or not to be" `shouldBe` ["to","be","or","not","to","be"]
             }
         }
     ; context "words . map toLower = map (map toLower) . words" $ do
         { it "が満たされているはず．" $ property $ \ strs ->
             (words . map toLower) (unwords strs) == (map (map toLower) . words) (unwords (strs :: [String]))
         }
     }

