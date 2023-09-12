{-# LANGUAGE UndecidableInstances #-}

module Sol.Types
  ( SolVal (..),
    SolType (..),
    SolPacked (..),
    isDyn,
  )
where

data SolType
  = ArrayDyn SolType
  | Array Int SolType
  | Tuple [SolType]
  | UInt
  deriving (Eq)

instance Show SolType where
  show UInt = "uint256"
  show (Tuple l) = "(" ++ f l ++ ")"
    where
      f [] = ""
      f [v] = show v
      f (v : vs) = show v ++ ", " ++ f vs
  show (Array l t) = show t ++ "[" ++ show l ++ "]"
  show (ArrayDyn t) = show t ++ "[]"

data SolVal
  = SArr [SolVal]
  | SInt Integer
  deriving (Eq, Show)

-- instance Show SolVal where
--   show (SInt i) = show i
--   show (SArr arr) = show arr

instance Semigroup SolVal where
  (SArr xs) <> (SArr ys) = SArr $ xs <> ys
  x <> (SArr ys) = SArr $ x : ys
  (SArr xs) <> y = SArr $ xs <> [y]
  x <> y = SArr [x, y]

instance Monoid SolVal where
  mempty = SArr []

data SolPacked = SolPacked SolType SolVal
  deriving (Show, Eq)

isDyn :: SolType -> Bool
isDyn (ArrayDyn _) = True
isDyn (Array _ t) = isDyn t
isDyn (Tuple ts) = any isDyn ts
isDyn _ = False
