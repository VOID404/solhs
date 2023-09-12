{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Sol.Coerce
  ( SolPacked (..),
    SolPack (..),
    SolPack1 (..),
  )
where

import Data.Maybe
import GHC.Generics hiding (UInt)
import Sol.Pack (pack', typePacker)
import Sol.Types

class SolPack a where
  solCoerce :: a -> SolVal
  default solCoerce :: (Generic a, SolPack1 (Rep a)) => a -> SolVal
  solCoerce = solCoerce1 . from

  pack :: SolType -> a -> Maybe SolPacked
  pack t v = (fmap (SolPacked t . snd) <$> pack' (typePacker t)) vals
    where
      vals = case solCoerce v of
        SArr l -> l
        v -> [v]

  unsafePack :: SolType -> a -> SolPacked
  unsafePack t = fromMaybe (error "Unpacked unsafely") . pack t

instance SolPack Integer where
  solCoerce = SInt

instance SolPack Int where
  solCoerce = SInt . toInteger

instance (SolPack a) => SolPack [a] where
  solCoerce = SArr . map solCoerce

-- solCoerce = solCoerce1 . from

class SolPack1 a where
  solCoerce1 :: a p -> SolVal

-- | Types without constructor
instance SolPack1 V1 where
  solCoerce1 _ = mempty

-- | Unit: used for constructors without arguments
instance SolPack1 U1 where
  solCoerce1 _ = mempty

-- | Products: encode multiple arguments to constructors
instance (SolPack1 a, SolPack1 b) => SolPack1 (a :*: b) where
  solCoerce1 (a :*: b) = solCoerce1 a <> solCoerce1 b

-- | Sums: encode choice between constructors
instance (SolPack1 a, SolPack1 b) => SolPack1 (a :+: b) where
  solCoerce1 (L1 x) = solCoerce1 x
  solCoerce1 (R1 x) = solCoerce1 x

-- | Meta-information (constructor names, etc.)
instance (SolPack1 a) => SolPack1 (M1 i c a) where
  solCoerce1 (M1 x) = solCoerce1 x

-- | Constants, additional parameters and recursion of kind *
instance (SolPack a) => SolPack1 (K1 i a) where
  solCoerce1 (K1 x) = case solCoerce x of
    SArr [v] -> v
    v@(SArr (_ : _ : _)) -> SArr [v]
    v -> v

instance (SolPack v1, SolPack v2) => SolPack (v1, v2)

instance (SolPack v1, SolPack v2, SolPack v3) => SolPack (v1, v2, v3)

instance (SolPack v1, SolPack v2, SolPack v3, SolPack v4) => SolPack (v1, v2, v3, v4)

-- instance (SolPack v1, SolPack v2, SolPack v3, SolPack v4, SolPack v5) => SolList (v1, v2, v3, v4, v5)
