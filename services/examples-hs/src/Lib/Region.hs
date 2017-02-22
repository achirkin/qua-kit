{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Lib.Region
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
-- Variation of a quad-tree.
-- Uses BoundedBy (cheap) and Spatial (expensive) type classes to locate objects in logarithmic time.
-- Constructing the whole tree from a list is O(n log n)
-- Does not support adding new objects yet.
--
-----------------------------------------------------------------------------

module Lib.Region
  ( -- * Data type
    Region ()
  , rBound
    -- ^ Everything in this region is fully contained within this bound
  , rCenter
    -- ^ Pivoting point to split the quad-tree into four branches
  , rBlocks, rLL, rLR, rUL, rUR
    -- * Object-location queries
  , findClosest, foldNearby
    -- * Construction
  , fromList, toList
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import qualified Control.Lens as Lens
import GHC.Exts (IsList(..))


import Numeric.EasyTensor
import Lib.ExtraTypes


-- | Data structure to store blocks.
--   Every child block gets the blocks with centers in corresponding quadrant of parent.
--   Though their bounding boxes may overlap a little bit.
--   Construction guarantees (fromList):
--    1. Bounds of objects on current level larger than half of region bound
--    2. Each existing leaf contains at least one object inside.
data Region a
  = Region
  { _rBound  :: !(MinMax Vec2f)
    -- ^ Everything in this region is fully contained within this bound
  , _rCenter :: !Vec2f
    -- ^ Pivoting point to split the quad-tree into four branches
  , _rBlocks :: ![a]
    -- ^ All the blocks big enough to not fit within child bounded regions
  , _rLL     :: !(Region a)
    -- ^ Lower-left region
  , _rLR     :: !(Region a)
    -- ^ Lower-right region
  , _rUL     :: !(Region a)
    -- ^ Upper-left region
  , _rUR     :: !(Region a)
    -- ^ Upper-right region
  }
  | Wasteland
  { _rBound  :: !(MinMax Vec2f)
    -- ^ Everything in this region is fully contained within this bound
  , _rCenter :: !Vec2f
    -- ^ Pivoting point to split the quad-tree into four branches
  }
  deriving (Eq,Show)

instance BoundedBy Vec2f (Region a) where
  bounds = _rBound
  {-# INLINE bounds #-}

instance Functor Region where
  fmap f Region {..} = Region
    { _rBound  = _rBound
    , _rCenter = _rCenter
    , _rBlocks = f <$> _rBlocks
    , _rLL     = f <$> _rLL
    , _rLR     = f <$> _rLR
    , _rUL     = f <$> _rUL
    , _rUR     = f <$> _rUR
    }
  fmap _ Wasteland {..} = Wasteland _rBound _rCenter

instance Foldable Region where
  foldMap f Region {..} = foldMap f _rBlocks
                `mappend` foldMap f _rLL
                `mappend` foldMap f _rLR
                `mappend` foldMap f _rUL
                `mappend` foldMap f _rUR
  foldMap _ Wasteland {} = mempty

instance Traversable Region where
  traverse f Region {..} = Region _rBound _rCenter
      <$> traverse f _rBlocks
      <*> traverse f _rLL
      <*> traverse f _rLR
      <*> traverse f _rUL
      <*> traverse f _rUR
  traverse _ Wasteland {..} = pure $ Wasteland _rBound _rCenter
  sequenceA Region {..} = Region _rBound _rCenter
      <$> sequenceA _rBlocks
      <*> sequenceA _rLL
      <*> sequenceA _rLR
      <*> sequenceA _rUL
      <*> sequenceA _rUR
  sequenceA Wasteland {..} = pure $ Wasteland _rBound _rCenter

instance BoundedBy Vec2f a => IsList (Region a) where
  type Item (Region a) = a
  fromList xs = foldl' (addObject $ mmDiff scBound) (Wasteland scBound scCenter) xs
    where
      scBound' = sconcat . ( mmBound :|) $ map bounds xs
      scCenter = mmAvg scBound'
      scDist   = broadcast . unScalar $ normLPInf (mmDiff scBound') / 2
      scBound  = MinMax (scCenter - scDist) (scCenter + scDist)
      addObject softSize r@Region{..} b
        | d <- softSize * 0.5
        , bc <- mmAvg $ bounds b
        , mmDiff (bounds b) < d =
            case (_x bc <= _x _rCenter, _y bc <= _y _rCenter) of
              (True, True) -> r
                { _rBound = _rBound <> bounds b
                , _rLL     = addObject d _rLL b
                }
              (True, False) -> r
                { _rBound = _rBound <> bounds b
                , _rUL     = addObject d _rUL b
                }
              (False, True) -> r
                { _rBound = _rBound <> bounds b
                , _rLR     = addObject d _rLR b
                }
              (False, False) -> r
                { _rBound = _rBound <> bounds b
                , _rUR     = addObject d _rUR b
                }
        | otherwise = r{ _rBound = _rBound <> bounds b, _rBlocks = b:_rBlocks}
      addObject softSize Wasteland {..} b
        | d <- softSize * 0.5
        , MinMax bmin bmax <- _rBound
        , dx <- vec2 (realToFrac $ _x d) 0
        , dy <- vec2 0 (realToFrac $ _y d) = addObject softSize
              Region
                { _rBound  = _rBound
                , _rCenter = _rCenter
                , _rBlocks = []
                , _rLL     = Wasteland (MinMax bmin (bmax-d))       (_rCenter - d/2)
                , _rUL     = Wasteland (MinMax (bmin+dy) (bmax-dx)) (_rCenter - dx/2 + dy/2)
                , _rLR     = Wasteland (MinMax (bmin+dx) (bmax-dy)) (_rCenter + dx/2 - dy/2)
                , _rUR     = Wasteland (MinMax (bmin+d) bmax)       (_rCenter + d/2)
                }
              b
  toList Region {..} = _rBlocks ++ toList _rLL ++ toList _rLR ++ toList _rUL ++ toList _rUR
  toList Wasteland {} = []




closerObject :: Spatial 2 Float a => Vec2f -> Option (ArgMin Scf a) -> a -> Option (ArgMin Scf a)
closerObject p (Option Nothing) b = Option . Just . Min $ Arg (p `distL2To` b) b
closerObject p (Option (Just ax@(Min (Arg x _)))) b =
    if normL1 y - normL1 dy * 0.5 > x * 1.4142135623730951
    then Option (Just ax)
    else Option . Just $ ax <> Min (Arg (p `distL2To` b) b)
  where
    y  = mmAvg (bounds b) - p
    dy = mmDiff (bounds b) :: Vec2f

closerObjectInR ::Spatial 2 Float a => Vec2f -> Option (ArgMin Scf a) -> Region a -> Option (ArgMin Scf a)
closerObjectInR _ x Wasteland {} = x
closerObjectInR p (Option (Just ax@(Min (Arg x _)))) Region {..}
    | y <- mmAvg _rBound - p
    , dy <- mmDiff _rBound
    , normL1 y - normL1 dy * 0.5 > x * 1.4142135623730951 = Option (Just ax)
closerObjectInR p ax Region {..} =
    case (_x p <= _x _rCenter, _y p <= _y _rCenter) of
      (True , True ) -> ax' +^+ _rLL +^+ _rUL +^+ _rLR +^+ _rUR
      (True , False) -> ax' +^+ _rUL +^+ _rLL +^+ _rUR +^+ _rLR
      (False, True ) -> ax' +^+ _rLR +^+ _rLL +^+ _rUR +^+ _rUL
      (False, False) -> ax' +^+ _rUR +^+ _rUL +^+ _rLR +^+ _rLL
  where
    -- find a closer block in a given region
    infixl 3 +^+
    ob +^+ r' = closerObjectInR p ob r'
    -- closest block on this level, if any
    ax' = foldl' (closerObject p) ax _rBlocks


-- | Find the closest object inside the region.
--   O(log n) complexity.
findClosest :: Spatial 2 Float a => Vec2f -> Region a -> Maybe (Arg Scf a)
findClosest p = fmap getMin . getOption . closerObjectInR p (Option Nothing)


-- | Fold objects within a given boundary.
--   Should be something like O(k log n) where k is the number of objects in a region.
foldNearby :: (BoundedBy Vec2f a, Monoid m) => (a -> m) -> MinMax Vec2f -> Region a -> m
foldNearby _ _ Wasteland {} = mempty
foldNearby f bound@(MinMax bll bur) Region {..}
    | MinMax rll rur <- _rBound
    , bur > rll && bll < rur = foldMap f' _rBlocks +^+ _rLL +^+ _rUL +^+ _rLR +^+ _rUR
    | otherwise = mempty
  where
    -- go recursive into children
    infixl 3 +^+
    m +^+ r' = m `mappend` foldNearby f bound r'
    f' i | MinMax ill iur <- bounds i
         , bur > ill && bll < iur = f i
         | otherwise              = mempty



Lens.makeLenses ''Region
