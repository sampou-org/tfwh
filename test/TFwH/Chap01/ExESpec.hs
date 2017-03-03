{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module TFwH.Chap01.ExESpec where

import Data.Typeable

import TFwH.Chap01.ExE
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "第1章練習問題E" $ do
     { context "(x + y) + z = x + (y + z)" $ do
         { it "が満たされているはず．" $ property $ \ (x,y,z)  ->
             (x + y) + z == (x :: Int) + (y + z)
         }
     ; context "0 + y = y" $ do
         { it "が満たされているはず．" $ property $ \ y  ->
             0 + y == (y :: Int)
         }
     ; context "x + 0 = x" $ do
         { it "が満たされているはず．" $ property $ \ x  ->
             x + 0 == (x :: Int)
         }
     ; context "(xs ++ ys) ++ zs = xs ++ (ys ++ zs)" $ do
         { it "が満たされているはず．" $ property $ \ (xs,ys,zs) ->
             (xs ++ ys) ++ zs == (xs :: String) ++ (ys ++ zs)
         }
     ; context "[] ++ ys = ys" $ do
         { it "が満たされているはず．" $ property $ \ ys ->
             [] ++ ys == (ys :: String)
         }
     ; context "xs + [] = xs" $ do
         { it "が満たされているはず．" $ property $ \ xs ->
             xs ++ [] == (xs :: String)
         }
     ; context "(f . g) . h = f . (g . h)" $ do
         { it "が満たされているはず．" $ property $ \ (f::Fun,g::Fun,h::Fun,x::Int) ->
             ((f . g) . h) x == (f . (g . h)) x 
         }
     ; context "id . g = g" $ do
         { it "が満たされているはず．" $ property $ \ (g,x) ->
             (id . g) x == (g :: Fun) (x :: Int)
         }
     ; context "id . g = g" $ do
         { it "が満たされているはず．" $ property $ \ (g,x) ->
             (id . g) x == (g :: Fun) (x :: Int)
         }
     }
