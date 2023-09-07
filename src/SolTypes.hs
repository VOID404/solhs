{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module SolTypes (
  SolVal (SArr, SInt),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace (traceShow)
import GHC.Generics hiding (UInt)
import Numeric (showHex)

data SolType
  = ArrayDyn SolType
  | Array Int SolType
  | Tuple [SolType]
  | UInt
  deriving (Show)

data SolVal
  = SArr [SolVal]
  | SInt Int
  deriving (Show)

data SolPacked = SolPacked SolType SolVal
  deriving (Show)

class SolPack a where
  pack :: SolType -> a -> SolPacked

  packVal :: SolType -> a -> SolVal
  packVal t v =
    let SolPacked _ val = pack t v
     in val

instance SolPack Int where
  pack :: SolType -> Int -> SolPacked
  pack t@UInt n = SolPacked t $ SInt n
  pack t n = error $ "Invalid Solidity type for " ++ show n ++ ": " ++ show t

instance (SolPack a) => SolPack [a] where
  pack :: SolType -> [a] -> SolPacked
  pack t@(ArrayDyn vt) vs = SolPacked t $ SArr $ map (packVal vt) vs
  pack t@(Array l vt) vs
    | l /= length vs = error $ "Invalid array length [expected != actual]: " ++ show l ++ " != " ++ show (length vs)
    | otherwise = SolPacked t $ SArr $ map (packVal vt) vs
  pack t _ = error $ "Invalid Solidity type for array: " ++ show t

isDyn :: SolType -> Bool
isDyn (ArrayDyn _) = True
isDyn (Array _ t) = isDyn t
isDyn (Tuple ts) = any isDyn ts
isDyn _ = False

byteLen :: Text -> Int
byteLen = (`div` 2) . T.length

enc :: SolPacked -> Text
enc (SolPacked UInt (SInt n)) = T.justifyRight 64 '0' $ T.pack $ showHex n ""
enc (SolPacked (Tuple ts) (SArr arr)) = T.append (T.concat heads) (T.concat tails)
 where
  packed = zipWith SolPacked ts arr
  tails = map tail packed
   where
    tail :: SolPacked -> Text
    tail p@(SolPacked t _)
      | isDyn t = enc p
      | otherwise = ""
  headLen = sum $ map headl packed
   where
    headl :: SolPacked -> Int
    headl p@(SolPacked t _)
      | isDyn t = 32
      | otherwise = byteLen $ enc p
  heads = zipWith head [0 ..] packed
   where
    head :: Int -> SolPacked -> Text
    head i p@(SolPacked t _)
      | isDyn t = enc $ pack UInt $ headLen + sum (map byteLen $ take i tails)
      | otherwise = enc p
enc (SolPacked (Array l t) arr@(SArr _)) = enc $ SolPacked (Tuple ts) arr
 where
  ts = replicate l t
enc (SolPacked (ArrayDyn t) arr@(SArr vs)) = len `T.append` val
 where
  len = enc $ pack UInt $ length vs
  ts = map (const t) vs
  val = enc $ SolPacked (Tuple ts) arr
enc v = error $ "Invalid packed data: " ++ show v

instance (SolPack a, SolPack b) => SolPack (a, b)
instance (SolPack a, SolPack b, SolPack c) => SolPack (a, b, c)
instance (SolPack a, SolPack b, SolPack c, SolPack d) => SolPack (a, b, c, d)
instance (SolPack a, SolPack b, SolPack c, SolPack d, SolPack e) => SolPack (a, b, c, d, e)
instance (SolPack a, SolPack b, SolPack c, SolPack d, SolPack f, SolPack g) => SolPack (a, b, c, d, f, g)

sampleg = mapM_ T.putStrLn $ T.chunksOf 64 $ enc $ pack (Tuple [UInt, UInt, UInt]) ((1, 2, 3) :: (Int, Int, Int))

-- sample = mapM_ T.putStrLn $ T.chunksOf 64 $ enc tpl
--   where
--     arrType = Array 4 (ArrayDyn UInt)
--     num = packVal UInt (10 :: Int)
--     arr = packVal arrType ([[3, 4], [5, 6], [7, 8], [9, 10]] :: [[Int]])
--     tpl = SolPacked (Tuple [UInt, UInt, Array 4 (ArrayDyn UInt)]) $ SArr [num, num, arr]

sample = mapM_ T.putStrLn $ T.chunksOf 64 encoded -- some magic to make it print nicely
 where
  -- there is some native representation - with native type Int
  num = 10 :: Int

  -- same with arrays
  arr = [[3, 4], [5, 6], [7, 8], [9, 10]] :: [[Int]]

  -- you can represent Solidity types in a native way, eg uint256[][4]
  arrType = Array 4 (ArrayDyn UInt)
  -- or (uint256, uint256, uint256[][4])
  tplType = Tuple [UInt, UInt, arrType]

  -- some types can be packed semi-automatically
  arrPacked = packVal arrType arr
  numPacked = packVal UInt num

  -- I didn't want to deal with tuples, so it has to be packed manually
  tplPacked = SolPacked tplType (SArr [numPacked, numPacked, arrPacked])

  -- packed types can be encoded
  encoded = enc tplPacked