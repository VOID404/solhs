{-# LANGUAGE DeriveGeneric #-}

module Sol.CoerceSpec (spec) where

import GHC.Generics (Generic)
import Sol.Coerce (SolPack (solCoerce))
import Sol.Types (SolVal (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

newtype U = U Integer
  deriving (Show, Generic)

instance SolPack U

data P = P Int Int Int [Int]
  deriving (Show, Generic)

instance SolPack P

testCase :: (Show a, SolPack a) => a -> SolVal -> SpecWith ()
testCase val res = it (show val) $ solCoerce val `shouldBe` res

spec :: Spec
spec = do
  describe "Int" $ do
    testCase
      (5 :: Int)
      (SInt 5)
    testCase
      [5 :: Int]
      (SArr [SInt 5])
  describe "Tuples" $ do
    testCase
      ((1, 2, 3) :: (Int, Integer, Int))
      (SArr $ map SInt [1, 2, 3])
    testCase
      ((1, (2, 3), 4) :: (Int, (Integer, Int), Integer))
      (SArr [SInt 1, SArr [SInt 2, SInt 3], SInt 4])

  describe "Generic" $ do
    testCase
      [U 1, U 2]
      (SArr [SInt 1, SInt 2])
    testCase
      (P 1 2 3 [4, 5, 6])
      (SArr [SInt 1, SInt 2, SInt 3, SArr [SInt 4, SInt 5, SInt 6]])
