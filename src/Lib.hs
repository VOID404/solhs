{-# LANGUAGE OverloadedStrings #-}

module Lib (
    someFunc,
    enc,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data SolType
    = ArrayDyn SolType [SolType]
    | Array SolType [SolType] -- Has to be same type
    | Tuple [SolType] -- Doesn't have to be same type
    | Uint256 Int -- I really don't want to deal with lengths for now...
    deriving (Show)

isDyn (ArrayDyn _ _) = True
isDyn (Array t _) = isDyn t
isDyn (Tuple ts) = any isDyn ts
isDyn _ = False

-- def tail(x):
--     if x.solType.isStatic():
--         return ""
--     else:
--         return enc(x)

-- def encTuple(x: list) -> str:
--     assert(len(x) >= 0)

--     tails = [tail(v) for v in x]
--     def headLen(v):
--         if v.solType.isStatic():
--             return len(enc(v)) # can be optimized
--         else:
--             return 256
--     headLens = [headLen(v) for v in x].sum()

--     def head(i, v):
--         if v.solType.isStatic():
--             return enc(v)
--         else:
--             enc(headLens + len(tails[1:i-1])) # as uint256

--     heads = [head(i, v) for i, v in x.enumerate()]

enc :: SolType -> Text
enc (Uint256 n) = T.justifyRight 64 '0' $ T.pack $ showHex n ""
enc (Tuple vs) = T.append (T.concat heads) (T.concat tails)
  where
    tail :: SolType -> Text
    tail v
        | isDyn v = enc v
        | otherwise = ""
    headLen v
        | isDyn v = 32
        | otherwise = T.length (enc v) `div` 2
    tails :: [Text]
    tails = map tail vs
    headLens = sum $ map headLen vs
    head i v
        | isDyn v = enc $ Uint256 $ headLens + sum (map ((`div` 2) . T.length) $ take (i - 1) tails)
        | otherwise = enc v
    heads :: [Text]
    heads = zipWith head [0 ..] vs
enc (Array _ vs) = enc $ Tuple vs
enc (ArrayDyn _ vs) = enc (Uint256 $ length vs) `T.append` enc (Tuple vs)

sampleStaticArray = Array (Uint256 0) [Uint256 1, Uint256 2, Uint256 3, Uint256 4]
sampleDynArray = ArrayDyn (Uint256 0) [Uint256 1, Uint256 2, Uint256 3, Uint256 4]
sampleStaticTuple = Tuple [Uint256 1, Uint256 2, Tuple [Uint256 3, Uint256 4], Uint256 5]
sampleDynamicTuple = Tuple [Uint256 1, Uint256 2, ArrayDyn (Uint256 0) [Uint256 3, Uint256 4], Uint256 5]

-- uint256,uint256,uint256[][4]
-- with data
-- 	10,10,
-- 	[[3,4],[5,6],[7,8],[9,10]]

dubbaData =
    Tuple
        [ Uint256 10
        , Uint256 10
        , Array
            (ArrayDyn (Uint256 0) [])
            [ ArrayDyn (Uint256 0) [Uint256 3, Uint256 4]
            , ArrayDyn (Uint256 0) [Uint256 5, Uint256 6]
            , ArrayDyn (Uint256 0) [Uint256 7, Uint256 8]
            , ArrayDyn (Uint256 0) [Uint256 9, Uint256 10]
            ]
        ]

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- (uint256, uint256, uint256[], uint256)
-- (1, 2, [3, 4], 5)

-- 0000000000000000000000000000000000000000000000000000000000000001
-- 0000000000000000000000000000000000000000000000000000000000000002
-- 0000000000000000000000000000000000000000000000000000000000000080
-- 0000000000000000000000000000000000000000000000000000000000000005
-- 0000000000000000000000000000000000000000000000000000000000000002
-- 0000000000000000000000000000000000000000000000000000000000000003
-- 0000000000000000000000000000000000000000000000000000000000000004