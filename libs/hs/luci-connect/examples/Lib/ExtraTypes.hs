{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Lib.ExtraTypes
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-----------------------------------------------------------------------------

module Lib.ExtraTypes
  ( MinMax (..), minMax, mmBound, mmDiff, mmAvg
  , BoundedBy (..), InBounds (..), unBound
  , Spatial (..), ClusterId
  , _x, _y, fill, dimN, dimM, dimK
  ) where

import Control.Monad.Fix
import Data.Semigroup
import Numeric.EasyTensor
import GHC.Num (Num(..))
import Data.Proxy
import GHC.TypeLits (natVal, KnownNat)

-- | Another name for cluster ids, based on Int type.
newtype ClusterId = ClusterId Int
  deriving (Eq, Show, Ord, Num, Integral, Real, Enum)

-- Boundary type

data MinMax a = MinMax !a !a
  deriving (Eq, Read, Show)

minMax :: a -> MinMax a
minMax a = MinMax a a

mmDiff :: Num a => MinMax a -> a
mmDiff (MinMax x y) = y - x

mmAvg :: Fractional a => MinMax a -> a
mmAvg (MinMax x y) = 0.5 * (x+y)

instance (Ord a, Bounded a) => Monoid (MinMax a) where
  mempty = MinMax minBound maxBound
  mappend = (<>)

instance Ord a => Semigroup (MinMax a) where
  (MinMax x1 y1) <> (MinMax x2 y2) = MinMax (min x1 x2) (max y1 y2)

instance Functor MinMax where
  fmap f (MinMax a b) = MinMax (f a) (f b)

instance Applicative MinMax where
  pure a = MinMax a a
  MinMax f g <*> MinMax a b = MinMax (f a) (g b)

instance Monad MinMax where
  return = pure
  MinMax a b >>= m = case (m a, m b) of
      (MinMax x _, MinMax _ y) -> MinMax x y

instance MonadFix MinMax where
  mfix mf = let MinMax x _ = mf x
                MinMax _ y = mf y
            in MinMax x y

instance Bounded a => Bounded (MinMax a) where
  minBound = MinMax minBound minBound
  maxBound = MinMax maxBound maxBound

-- | MinMax checks whether bounds overlap
instance Ord a => Ord (MinMax a) where
  MinMax _ y1 < MinMax x2 _ = y1 < x2
  MinMax x1 _ > MinMax _ y2 = x1 > y2
  MinMax _ y1 <= MinMax _ y2 = y1 <= y2
  MinMax x1 _ >= MinMax x2 _ = x1 >= x2
  compare = undefined
  min (MinMax x1 y1) (MinMax x2 y2) = MinMax (min x1 x2) (min y1 y2)
  max (MinMax x1 y1) (MinMax x2 y2) = MinMax (max x1 x2) (max y1 y2)

instance (Num a, Ord a) => Num (MinMax a) where
  MinMax x1 y1 + MinMax x2 y2 = MinMax (x1+x2) (y1+y2)
  MinMax x1 y1 - MinMax x2 y2 = MinMax (x1-y2) (y1-x2)
  MinMax x1 y1 * MinMax x2 y2 = MinMax (x1*x2) (y1*y2)
  abs (MinMax x y) = let ax = abs x
                         ay = abs y
                     in MinMax (min ax ay) (max ax ay)
  negate (MinMax x y) = MinMax (negate y) (negate x)
  signum (MinMax x y) = MinMax (signum x) (signum y)
  fromInteger i = let x = fromInteger i in MinMax x x



class BoundedBy b a where
  -- | get bounds for a given data type
  bounds :: a -> MinMax b

instance BoundedBy a (MinMax a) where
  bounds = id
  {-# INLINE bounds #-}

-- | Data type together with bounds
data InBounds a = InBounds !(MinMax a) !a

instance BoundedBy a (InBounds a) where
  bounds (InBounds mm _) = mm

-- | get InBounds value
unBound :: InBounds a -> a
unBound (InBounds _ a) = a

instance Bounded a => Bounded (InBounds a) where
  minBound = InBounds (MinMax minBound maxBound) minBound
  maxBound = InBounds (MinMax minBound maxBound) maxBound




-- | A class of volumentric objects in a metric space
class BoundedBy (Vector t n) a => Spatial n t a where
  -- | Test if a point is inside a spatial object
  isInside :: Vector t n -> a -> Bool
  -- | Closest distance from point to a spatial object
  distL2To :: Vector t n -> a -> Scalar t
  default distL2To :: (Floating t) => Vector t n -> a -> Scalar t
  distL2To p = sqrt . distL2SquaredTo p
  {-# INLINE distL2To #-}
  -- | Closest distance from point to a spatial object (squared)
  distL2SquaredTo   :: Vector t n -> a -> Scalar t

instance BoundedBy (Vector t n) (Vector t n) where
  bounds a = MinMax a a

instance ( Floating t
         , ElementWise (Idx '[n]) t (Vector t n)
         , ElementWise (Idx '[]) t (Scalar t)
         , Num (Vector t n)
         ) => Spatial n t (Vector t n) where
  isInside _ _ = False
  distL2SquaredTo p q = case p - q of d -> dot d d



fill :: ElementWise (Idx ds) t (Tensor t ds)
     => Scalar t -> Tensor t ds
fill = broadcast . unScalar

dimN :: KnownNat n => p (n ': ns) -> Int
dimN = fromInteger . natVal . f
  where
    f :: p (n ': ns) -> Proxy (n :: Nat)
    f _ = Proxy

dimM :: KnownNat m => p (n ': m ': ns) -> Int
dimM = fromInteger . natVal . f
  where
    f :: p (n ': m ': ns) -> Proxy (m :: Nat)
    f _ = Proxy


dimK :: KnownNat k => p (n ': m ': k ': ns) -> Int
dimK = fromInteger . natVal . f
  where
    f :: p (n ': m ': k ': ns) -> Proxy (k :: Nat)
    f _ = Proxy

-- Vec2f helpers

mmBound :: MinMax Vec2f
mmBound = let z = broadcast (read "Infinity") in MinMax z (-z)



_x :: Vec2f -> Scf
_x = scalar . (! 1)

_y :: Vec2f -> Scf
_y = scalar . (! 2)
