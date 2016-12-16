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
  , _x, _y
  ) where

import Control.Monad.Fix
import Data.Semigroup
import Numeric.EasyTensor


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


-- Vec2f helpers

mmBound :: MinMax Vec2f
mmBound = let z = fill (scalar $ read "Infinity") in MinMax z (-z)


_x :: Vec2f -> Scf
_x = index 1 1

_y :: Vec2f -> Scf
_y = index 2 1
