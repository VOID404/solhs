{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Sol.Pack
  ( typePacker,
    pack',
  )
where

import Control.Applicative (Alternative (many), Applicative (liftA2), empty, (<|>))
import Sol.Types

newtype Packer i o = Packer {pack' :: i -> Maybe (i, o)}

instance Functor (Packer i) where
  fmap f packer = Packer $ fmap (fmap f) . pack' packer

instance Applicative (Packer i) where
  pure x = Packer $ pure . (,x)
  pf <*> po = Packer $ \input -> case pack' pf input of
    Nothing -> Nothing
    Just (rest, f) -> fmap f <$> pack' po rest

instance Alternative (Packer i) where
  empty = Packer $ const empty
  p1 <|> p2 = Packer $ \input -> pack' p1 input <|> pack' p2 input

sint :: Packer [SolVal] SolVal
sint = Packer $ \case
  (x@(SInt _) : xs) -> Just (xs, x)
  _ -> Nothing

repeatPack :: Int -> Packer a b -> Packer a [b]
repeatPack 0 _ = pure []
repeatPack n p = liftA2 (:) p (repeatPack (n - 1) p)

takeArr :: Packer [SolVal] b -> Packer [SolVal] b
takeArr p = Packer $ \case
  (SArr x : xs) -> (xs,) . snd <$> pack' p x
  xs -> pack' p xs

sarr :: Int -> SolType -> Packer [SolVal] SolVal
sarr 0 _ = Packer $ \v -> Just (v, SArr [])
sarr len t = SArr <$> repeatPack len tp
  where
    tp = typePacker t

sarrdyn :: SolType -> Packer [SolVal] SolVal
sarrdyn t = SArr <$> many tp
  where
    tp = typePacker t

stpl :: [SolType] -> Packer [SolVal] SolVal
stpl l = SArr <$> traverse typePacker l

typePacker :: SolType -> Packer [SolVal] SolVal
typePacker UInt = sint
typePacker (Array l t) = takeArr (sarr l t) <|> sarr l t
typePacker (ArrayDyn t) = takeArr (sarrdyn t) <|> sarrdyn t
typePacker (Tuple l) = takeArr $ stpl l

-- digit1 :: Parser String Int
-- digit1 = Parser $ \i -> case runParser (satisfy isDigit) i of
--   Nothing      -> Nothing
--   Just (i', o) -> Just (i', digitToInt o)
