{-# LANGUAGE OverloadedStrings #-}

module Sol.Encode
  ( enc,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import Sol.Coerce
import Sol.Types

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
          | isDyn t = enc $ unsafePack UInt $ headLen + sum (map byteLen $ take i tails)
          | otherwise = enc p
enc (SolPacked (Array l t) arr@(SArr _)) = enc $ SolPacked (Tuple ts) arr
  where
    ts = replicate l t
enc (SolPacked (ArrayDyn t) arr@(SArr vs)) = len `T.append` val
  where
    len = enc $ unsafePack UInt $ length vs
    ts = map (const t) vs
    val = enc $ SolPacked (Tuple ts) arr
enc v = error $ "Invalid packed data: " ++ show v
