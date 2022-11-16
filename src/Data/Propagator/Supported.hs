{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Propagator.Supported where

import Control.Applicative
import Data.HashSet
import Data.Propagator.Class
import Data.Propagator.Name

data Supported a = Supported !(HashSet Name) a
  deriving (Functor, Foldable, Traversable, Show)

instance Eq a => Eq (Supported a) where
  (==) :: Eq a => Supported a -> Supported a -> Bool
  Supported _ a == Supported _ b = a == b

instance Ord a => Ord (Supported a) where
  compare :: Ord a => Supported a -> Supported a -> Ordering
  Supported _ a `compare` Supported _ b = compare a b

instance Applicative Supported where
  pure :: a -> Supported a
  pure = Supported mempty
  (<*) :: Supported a -> Supported b -> Supported a
  Supported xs a <*  Supported ys _ = Supported (union xs ys) a
  (*>) :: Supported a -> Supported b -> Supported b
  Supported xs _  *> Supported ys b = Supported (union xs ys) b
  (<*>) :: Supported (a -> b) -> Supported a -> Supported b
  Supported xs f <*> Supported ys a = Supported (union xs ys) (f a)

instance Monad Supported where
  return :: a -> Supported a
  return = Supported mempty
  (>>) :: Supported a -> Supported b -> Supported b
  (>>) = (*>)
  (>>=) :: Supported a -> (a -> Supported b) -> Supported b
  Supported xs a >>= f = case f a of
    Supported ys b -> Supported (union xs ys) b

instance Propagated a => Propagated (Supported a) where
  merge :: Propagated a => Supported a -> Supported a -> Change (Supported a)
  merge (Supported xs a) (Supported ys b) = case merge a b of
    Change False c     -> Change False (Supported xs c)
    Change True c      -> Change True  (Supported (union xs ys) c)
    Contradiction zs s -> Contradiction (zs `union` xs `union` ys) s

instance Num a => Num (Supported a) where
  (+) :: Num a => Supported a -> Supported a -> Supported a
  (+) = liftA2 (+)
  (-) :: Num a => Supported a -> Supported a -> Supported a
  (-) = liftA2 (-)
  (*) :: Num a => Supported a -> Supported a -> Supported a
  (*) = liftA2 (*)
  abs :: Num a => Supported a -> Supported a
  abs = fmap abs
  signum :: Num a => Supported a -> Supported a
  signum = fmap signum
  negate :: Num a => Supported a -> Supported a
  negate = fmap negate
  fromInteger :: Num a => Integer -> Supported a
  fromInteger = pure . fromInteger
  
instance Fractional a => Fractional (Supported a) where
  (/) :: Fractional a => Supported a -> Supported a -> Supported a
  (/) = liftA2 (/)
  recip :: Fractional a => Supported a -> Supported a
  recip = fmap recip
  fromRational :: Fractional a => Rational -> Supported a
  fromRational = pure . fromRational

instance Floating a => Floating (Supported a) where
  pi :: Floating a => Supported a
  pi = pure pi
  exp :: Floating a => Supported a -> Supported a
  exp = fmap exp
  log :: Floating a => Supported a -> Supported a
  log = fmap log
  sqrt :: Floating a => Supported a -> Supported a
  sqrt = fmap sqrt
  logBase :: Floating a => Supported a -> Supported a -> Supported a
  logBase = liftA2 logBase
  (**) :: Floating a => Supported a -> Supported a -> Supported a
  (**) = liftA2 (**)
  sin :: Floating a => Supported a -> Supported a
  sin = fmap sin
  cos :: Floating a => Supported a -> Supported a
  cos = fmap cos
  tan :: Floating a => Supported a -> Supported a
  tan = fmap tan
  asin :: Floating a => Supported a -> Supported a
  asin = fmap asin
  acos :: Floating a => Supported a -> Supported a
  acos = fmap acos
  atan :: Floating a => Supported a -> Supported a
  atan = fmap atan
  sinh :: Floating a => Supported a -> Supported a
  sinh = fmap sinh
  cosh :: Floating a => Supported a -> Supported a
  cosh = fmap cosh
  tanh :: Floating a => Supported a -> Supported a
  tanh = fmap tanh
  asinh :: Floating a => Supported a -> Supported a
  asinh = fmap asinh
  acosh :: Floating a => Supported a -> Supported a
  acosh = fmap acosh
  atanh :: Floating a => Supported a -> Supported a
  atanh = fmap atanh
