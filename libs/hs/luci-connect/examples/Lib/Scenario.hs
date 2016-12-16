-----------------------------------------------------------------------------
-- |
-- Module      :  Lib.Scenario
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
--
-- Keep a 2D scenario consisting of building blocks and walls.
-- Scenario is organized as a quad-tree.
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Lib.Scenario
  ( -- * Data types
    Block (..), bCenter, bWalls, bBound
  , Wall (..), wCenter, wOrientation
  , Region (..), rBound, rBlocks, rLL, rLR, rUL, rUR, rCenter, findClosest
  , Scenario
  , isInside
  , fromList, toList
  ) where

import Data.Maybe (fromJust)

import GHC.Exts (IsList(..))
--import Data.Text (Text)
--import qualified Data.Text
import Data.Semigroup
import Data.List (foldl')
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Control.Lens as Lens
--import Control.Lens.Operators ((%%=), (%=))

import Numeric.EasyTensor
import Lib.ExtraTypes

-- | A building block is defined by its center and a list of walls
data Block = Block
  { _bCenter :: !Vec2f
    -- ^ A center of a building block.
    --   A center coordinate must be equal to average of all corner points of a block.
  , _bWalls  :: ![Wall]
    -- ^ A list of all building walls.
    --   Must have at least three walls.
    --   All walls have relative coordinates w.r.t. to a building center.
    --   All walls must go in count-clock-wise order.
  , _bBound  :: !(MinMax Vec2f)
    -- ^ Minimum and maximum coordinates of a building block
  }
  deriving Show

-- | A single wall of a building is represented by a relative (w.r.t. building center) position
--   and orientation.
--   To the left of the orientation vector is always a solid - building block.
--   To the right of the orientation vector is always a void - empty space.
--   The width of a wall is two times the width of the orientation vector.
--   The position of a wall is a position of its center.
--   A wall has no width parameter.
data Wall = Wall
  { _wCenter :: !Vec2f
    -- ^ Position of a center of a wall relative to position of a parent object.
  , _wOrientation :: !Vec2f
    -- ^ Orientation of a wall.
    --   Direction of this vector represents CCW traverse of building walls.
    --   Size of this vector is a half of the wall width.
  }
  deriving Show


-- | Data structure to store blocks.
--   Every child block gets the blocks with centers in corresponding quadrant of parent.
--   Though their bounding boxes may overlap a little bit.
--   Construction guarantees (fromList):
--    1. Bounds of objects on current level larger than half of region bound
--    2. Each existing leaf contains at least one object inside.
data Region a = Region
  { _rBound  :: !(MinMax Vec2f)
  , _rCenter :: !Vec2f
    -- ^ Everything in this region is fully contained within this bound
  , _rBlocks :: ![a]
    -- ^ All the blocks big enough to not fit within child bounded regions
  , _rLL     :: !(Maybe (Region a))
    -- ^ Lower-left region
  , _rLR     :: !(Maybe (Region a))
    -- ^ Lower-right region
  , _rUL     :: !(Maybe (Region a))
    -- ^ Upper-left region
  , _rUR     :: !(Maybe (Region a))
    -- ^ Upper-right region
  } deriving (Eq,Show)


-- | All building blocks
type Scenario = Region Block




Lens.makeLenses ''Block
Lens.makeLenses ''Wall
Lens.makeLenses ''Region

----------------------------------------------------------------------------------------------------
-- * All the useful functions
----------------------------------------------------------------------------------------------------


-- | Test whether a point is under a building block
isInside :: Vec2f -> Block -> Bool
isInside p' bl | MinMax l u <- _bBound bl
               , p' >= l && p' <= u = ((1==) . (`mod` 2) . getSum)
                                    . foldMap (\w -> if intersects w then 1 else 0 :: Sum Int)
                                    $ _bWalls bl
               | otherwise          = False
  where
    p = p' - _bCenter bl
    ray = vec2 1 0
    intersects (Wall c o) | a <- c - o - p
                          , b <- c + o - p
                          , ra <- det2 ray a >= 0
                          , rb <- det2 ray b >= 0
                          , ab <- det2 a b > 0 = (ra /= rb) && (ra /= ab)


instance Functor Region where
  fmap f Region {..} = Region
    { _rBound  = _rBound
    , _rCenter = _rCenter
    , _rBlocks = f <$> _rBlocks
    , _rLL     = fmap f <$> _rLL
    , _rLR     = fmap f <$> _rLR
    , _rUL     = fmap f <$> _rUL
    , _rUR     = fmap f <$> _rUR
    }

instance Foldable Region where
  foldMap f Region {..} = foldMap f _rBlocks
                `mappend` foldMap (foldMap f) _rLL
                `mappend` foldMap (foldMap f) _rLR
                `mappend` foldMap (foldMap f) _rUL
                `mappend` foldMap (foldMap f) _rUR

instance Traversable Region where
  traverse f Region {..} = Region _rBound _rCenter
      <$> traverse f _rBlocks
      <*> traverse (traverse f) _rLL
      <*> traverse (traverse f) _rLR
      <*> traverse (traverse f) _rUL
      <*> traverse (traverse f) _rUR
  sequenceA Region {..} = Region _rBound _rCenter
      <$> sequenceA _rBlocks
      <*> sequenceA (fmap sequenceA _rLL)
      <*> sequenceA (fmap sequenceA _rLR)
      <*> sequenceA (fmap sequenceA _rUL)
      <*> sequenceA (fmap sequenceA _rUR)

instance IsList (Region Block) where
  type Item (Region Block) = Block
  fromList xs = fromJust $ foldl' (addBuilding scBound) Nothing xs
    where
      scBound = sconcat . ( mmBound :|) $ map _bBound xs
      addBuilding softBound@(MinMax bmin bmax) (Just r@Region{..}) b@Block{..}
        | d <- mmDiff softBound * 0.5
        , mmDiff _bBound < d
        , dx <- vec2 (realToFrac $ _x d) 0
        , dy <- vec2 0 (realToFrac $ _y d) = Just $
            case (_x _bCenter <= _x _rCenter, _y _bCenter <= _y _rCenter) of
              (True, True) -> r
                { _rBound = _rBound <> _bBound
                , _rLL     = addBuilding (MinMax bmin (bmax-d)) _rLL b
                }
              (True, False) -> r
                { _rBound = _rBound <> _bBound
                , _rUL     = addBuilding (MinMax (bmin+dy) (bmax-dx)) _rUL b
                }
              (False, True) -> r
                { _rBound = _rBound <> _bBound
                , _rLR     = addBuilding (MinMax (bmin+dx) (bmax-dy)) _rLR b
                }
              (False, False) -> r
                { _rBound = _rBound <> _bBound
                , _rUR     = addBuilding (MinMax (bmin+d) bmax) _rUR b
                }
        | otherwise = Just $ r{ _rBound = _rBound <> _bBound, _rBlocks = b:_rBlocks}
      addBuilding softBound Nothing b = addBuilding softBound (Just $ Region softBound (mmAvg softBound) [] Nothing Nothing Nothing Nothing) b
  toList Region {..} = _rBlocks ++ to' _rLL ++ to' _rLR ++ to' _rUL ++ to' _rUR
    where
      to' Nothing = []
      to' (Just x) = toList x

distToWall :: Vec2f -> Wall -> Scf
distToWall p Wall{..} = sqrt $ if between then d2 else min lu2 lv2
  where
    a = 2 * _wOrientation
    u = p - _wCenter + _wOrientation
    v = _wCenter + _wOrientation - p
    between = dot a u > 0 && dot a v > 0
    lu2 = dot u u
    lv2 = dot v v
    la2 = dot a a
    c = det2 a u
    d2 = c * c / la2

distToBlock :: Vec2f -> Block -> Scf
distToBlock p b = if p `isInside` b then 0
                                    else let p' = p - _bCenter b
                                         in getMin $ foldMap1 (Min . distToWall p') (Min (normL2 p')) (_bWalls b)


closerBlock :: Vec2f -> Option (ArgMin Scf Block) -> Block -> Option (ArgMin Scf Block)
closerBlock p (Option Nothing) b = Option . Just . Min $ Arg (distToBlock p b) b
closerBlock p (Option (Just ax@(Min (Arg x _)))) b =
    if let y = mmAvg (_bBound b) - p
           dy = mmDiff $ _bBound b
       in normL2 y - normL2 dy * 0.5 > x
    then Option (Just ax)
    else Option . Just $ ax <> Min (Arg (distToBlock p b) b)

closerBlockInR :: Vec2f -> Option (ArgMin Scf Block) -> Region Block -> Option (ArgMin Scf Block)
closerBlockInR p (Option (Just ax@(Min (Arg x _)))) Region {..}
    | y <- mmAvg _rBound - p
    , dy <- mmDiff _rBound
    , normL2 y - normL2 dy * 0.5 > x = Option (Just ax)
closerBlockInR p ax Region {..} =
    case (_x p <= _x _rCenter, _y p <= _y _rCenter) of
      (True , True ) -> ax' +^+ _rLL +^+ _rUL +^+ _rLR +^+ _rUR
      (True , False) -> ax' +^+ _rUL +^+ _rLL +^+ _rUR +^+ _rLR
      (False, True ) -> ax' +^+ _rLR +^+ _rLL +^+ _rUR +^+ _rUL
      (False, False) -> ax' +^+ _rUR +^+ _rUL +^+ _rLR +^+ _rLL
  where
    -- find a closer block in a given region
    infixl 3 +^+
    ob +^+ Nothing  = ob
    ob +^+ (Just r') = closerBlockInR p ob r'
    -- closest block on this level, if any
    ax' = foldl' (closerBlock p) ax _rBlocks


-- | Find closest object inside the region
findClosest :: Vec2f -> Region Block -> Maybe (Arg Scf Block)
findClosest p = fmap getMin . getOption . closerBlockInR p (Option Nothing)

foldMap1 :: (Semigroup m, Foldable t) => (a -> m) -> m -> t a -> m
foldMap1 f = F.foldl' (\m a -> m <> f a)

