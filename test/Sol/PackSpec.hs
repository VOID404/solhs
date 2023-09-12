module Sol.PackSpec (spec) where

import Sol.Coerce (SolPack, SolPacked (SolPacked), pack)
import Sol.Types (SolType (..), SolVal (..))
import Test.Hspec

packTest :: (Show a, SolPack a) => SolType -> a -> Maybe SolVal -> SpecWith ()
packTest t v r =
  it (show v ++ " -> " ++ show t) $
    pack t v `shouldBe` SolPacked t <$> r

spec :: Spec
spec = do
  describe "Int" $ do
    packTest UInt (4 :: Int) (Just $ SInt 4)
    packTest UInt [4 :: Int] (Just $ SInt 4)
    packTest UInt [[4 :: Int]] Nothing
  describe "tuples" $ do
    packTest
      (Tuple [UInt, UInt, UInt])
      ((1, 2, 3) :: (Int, Integer, Int))
      (Just $ SArr [SInt 1, SInt 2, SInt 3])
    packTest
      (Tuple [UInt, Tuple [UInt, UInt], UInt])
      ((1, (2, 3), 4) :: (Int, (Integer, Int), Int))
      (Just $ SArr [SInt 1, SArr [SInt 2, SInt 3], SInt 4])
  describe "arrays" $ do
    packTest
      (Array 4 UInt)
      ([1, 2, 3, 4] :: [Int])
      (Just $ SArr (map SInt [1, 2, 3, 4]))
    packTest
      (ArrayDyn UInt)
      ([1, 2, 3, 4] :: [Int])
      (Just $ SArr (map SInt [1, 2, 3, 4]))
    packTest
      (ArrayDyn UInt)
      ([[1, 2, 3, 4], [5, 6, 7]] :: [[Int]])
      (Just $ SArr (map SInt [1, 2, 3, 4]))
    packTest
      (Array 3 $ ArrayDyn UInt)
      ([[1, 2], [3, 4], [5, 6], [7, 8]] :: [[Int]])
      ( Just $
          SArr
            [ SArr [SInt 1, SInt 2],
              SArr [SInt 3, SInt 4],
              SArr [SInt 5, SInt 6]
            ]
      )
    packTest
      (ArrayDyn $ Array 2 UInt)
      ([[1, 2], [3, 4], [5, 6], [7, 8]] :: [[Int]])
      ( Just $
          SArr
            [ SArr [SInt 1, SInt 2],
              SArr [SInt 3, SInt 4],
              SArr [SInt 5, SInt 6]
            ]
      )

-- prop "not nested list" $ \x ->
