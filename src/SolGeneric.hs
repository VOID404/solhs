module SolGeneric (

) where

import GHC.Generics
import SolTypes

class SolCoerce a where
    solCoerce :: a -> SolVal

class SolList f where
    listify' :: f a -> [SolVal]

-- | Unit: used for constructors without arguments
instance SolList U1 where
    listify' U1 = []

-- | Products: encode multiple arguments to constructors
instance (SolList a, SolList b) => SolList (a :*: b) where
    listify' (a :*: b) = listify' a ++ listify' b

-- | Sums: encode choice between constructors
instance (SolList a, SolList b) => SolList (a :+: b) where
    listify' (L1 x) = listify' x
    listify' (R1 x) = listify' x

-- | Meta-information (constructor names, etc.)
instance (SolList a) => SolList (M1 i c a) where
    listify' (M1 x) = listify' x

-- | Constants, additional parameters and recursion of kind *
instance (SolCoerce a) => SolList (K1 i a) where
    listify' (K1 x) = [solCoerce x]
