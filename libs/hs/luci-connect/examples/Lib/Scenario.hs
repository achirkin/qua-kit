-----------------------------------------------------------------------------
-- |
-- Module      :  Lib.Scenario
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  chirkin@arch.ethz.ch
--
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
  , Scenario, scenarioBound
  , Grid (..), createGrid, indexedMap
  , gPoints, gCorner, gScale, gN, gM, gDef
  , gPointPos, closestGP, indexGrid
  , isInside, drawBlocksIds, drawBlocksDists
  , updatePatch
  , fromList, toList
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isJust, fromMaybe, fromJust)

import GHC.Exts (IsList(..))
--import Data.Text (Text)
--import qualified Data.Text
import Data.Semigroup
import Data.List (foldl')
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified Control.Lens as Lens
--import Control.Lens.Operators ((%%=), (%=))
import qualified Control.Parallel.Strategies as PS

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


-- | A grid in R2 for rasterised view on scenario results.
--   A grid is always aligned with XY axes.
--   Position of a point on a grid is ((i-1)*gScale + x(gCorner), (j-1)*gScale + y(gCorner))
data Grid a = Grid
  { _gPoints :: !(IntMap (IntMap a))
    -- ^ matrix represents all points of a grid
  , _gCorner :: !Vec2f
    -- ^ position of lower-left coordinate
  , _gScale  :: !Scf
    -- ^ uniform scale of grid point coordinates
  , _gN      :: !Int
    -- ^ number of elements in x direction
  , _gM      :: !Int
    -- ^ number of elements in y direction
  , _gDef    :: !a
    -- ^ default value used when initializing
  }
  deriving Show

instance Functor Grid where
  fmap f g = Grid
   { _gPoints = fmap f <$> _gPoints g
   , _gCorner = _gCorner g
   , _gScale  = _gScale g
   , _gN      = _gN g
   , _gM      = _gM g
   , _gDef    = f (_gDef g)
   }

instance Foldable Grid where
  foldMap f Grid{_gPoints = pts} = foldMap (foldMap f) pts


indexGrid :: Grid a -> Int -> Int -> a
indexGrid g@Grid{ _gPoints = m} i j = fromMaybe (_gDef g) $ IntMap.lookup i m >>= IntMap.lookup j

-- | Get position of a point in R2 given grid coordinates
gPointPos :: Int -> Int -> Grid a -> Vec2f
gPointPos i j g = vec2 x y + _gCorner g
  where
    x = (realToFrac i - 1) * realToFrac (_gScale g)
    y = (realToFrac j - 1) * realToFrac (_gScale g)

-- | Get closest point on a grid, given a position in R2
getClosestGP :: Vec2f -> Grid a -> (Int, Int)
getClosestGP v g = (clamp (_gN g) i', clamp (_gM g) j')
  where
    p = (v - _gCorner g) / fill (_gScale g)
    i' = round $ _x p
    j' = round $ _y p
    clamp n = min n . max 1


-- | Get closest point on a grid, given a position in R2
getClosestValM :: Vec2f -> Grid a -> Maybe a
getClosestValM v g = do
    i <- clamp (_gN g) (round $ _x p)
    j <- clamp (_gM g) (round $ _y p)
    colI <- IntMap.lookup i (_gPoints g)
    IntMap.lookup j colI
  where
    p = (v - _gCorner g) / fill (_gScale g)
    clamp n i = if i < 1 || i > n then Nothing else Just i


-- | A lens on a closest point on a grid to a given point.
closestGP :: Functor f => Vec2f -> (a -> f a) -> Grid a -> f (Grid a)
closestGP x k g@Grid{ _gPoints = m} = upd <$> mk closestValM
  where
    mk Nothing = Nothing <$ k closestVal
    mk (Just v) = Just <$> k v
    closestValM = getClosestValM x g
    (i,j) = getClosestGP x g
    closestVal = fromMaybe (_gDef g) closestValM
    upd Nothing = g
    upd (Just v) = if isJust closestValM
                   then g{ _gPoints = IntMap.adjust (IntMap.adjust (const v) j) i m}
                   else g



Lens.makeLenses ''Block
Lens.makeLenses ''Wall
Lens.makeLenses ''Grid
Lens.makeLenses ''Region

----------------------------------------------------------------------------------------------------
-- * All the useful functions
----------------------------------------------------------------------------------------------------

-- | Get minimum and maximum coordinates of a scenario
scenarioBound :: Scenario -> MinMax Vec2f
scenarioBound = _rBound

-- | Create a rectangular grid.
--   Resolution is determined by an a number of points along X or Y dimension,
--   depending on along wich direction the scenario is larger.
createGrid :: a -- ^ default value
           -> Int -- ^ Number of points in a larger dimension
           -> Scenario -- ^ A set of buildings to rasterize
           -> Grid a -- ^ Result grid
createGrid def n' sc = Grid
   { _gPoints = IntMap.fromList $ zip [1..n] (repeat oneCol)
   , _gCorner = ll
   , _gScale  = scale
   , _gN      = n
   , _gM      = m
   , _gDef    = def
   }
  where
    MinMax ll ur = scenarioBound sc
    dd = ur - ll
    oneCol = IntMap.fromList $ zip [1..m] (repeat def)
    (scale, n, m) =
       let xd = _x dd
           yd = _y dd
           d  = max xd yd
           s  = d / realToFrac n'
           m' = round $ min xd yd / s
       in if xd >= yd then ( s, n', m')
                      else ( s, m', n')

-- | Map a function over a grid
indexedMap :: (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedMap f g = Grid
   { _gPoints = IntMap.fromList . (PS.withStrategy (PS.parList $ PS.evalTuple2 PS.rpar PS.rpar) .
                                     map (\ (i, m) -> (i, IntMap.mapWithKey (f i) m))
                                  )
                                . IntMap.toList $ _gPoints g
   , _gCorner = _gCorner g
   , _gScale  = _gScale g
   , _gN      = _gN g
   , _gM      = _gM g
   , _gDef    = f 0 0 (_gDef g)
   }


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

drawBlocksDists :: Scenario -> Grid a -> Grid Float
drawBlocksDists bs g = indexedMap f g
  where
    f i j _ = case findClosest (gPointPos i j g) bs of
                Nothing -> 0
                (Just (Arg x _)) -> realToFrac x

-- | [Terribly inefficient] put BlockIds on a raster grid
drawBlocksIds :: Scenario -> Grid a -> Grid Int
drawBlocksIds bs g = indexedMap f g
  where
    bs' = zip [1..] $ toList bs
    f 0 0 _ = 0
    f i j _ = isAnyInside (gPointPos i j g) bs'
    isAnyInside v ((i,x):xs) = if v `isInside` x then i else isAnyInside v xs
    isAnyInside _ [] = 0

-- | Update a certain range of a grid
updatePatch :: (Vec2f -> a -> a) -> MinMax Vec2f -> Grid a -> Grid a
updatePatch f (MinMax ll ur) grid = indexedMap g grid
  where
    (i1,j1) = getClosestGP ll grid
    (i2,j2) = getClosestGP ur grid
    g i j | i1 <= i && i <= i2 && j1 <= j && j <= j2 = f (gPointPos i j grid)
          | otherwise = id



----------------------------------------------------------------------------------------------------

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
        | mmDiff _bBound < 0.5 * mmDiff _rBound
        , d <- mmDiff softBound / 2
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
    if let y = _bCenter b - p
           dy = mmDiff $ _bBound b
       in dot y y - dot dy dy * 0.25 > x*x
    then Option (Just ax)
    else Option . Just $ ax <> Min (Arg (distToBlock p b) b)

closerBlockInR :: Vec2f -> Option (ArgMin Scf Block) -> Region Block -> Option (ArgMin Scf Block)
closerBlockInR p (Option (Just ax)) Region {..}
    | d <- mmDiff _rBound * 1.5
    , not (_rCenter-d <= p && p <= _rCenter+d) = Option (Just ax)
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


--findClosest :: Vec2f -> Region Block -> Option (ArgMin Block Scf)
--findClosest p Region {..} | d4 <- mmDiff _rBound / 2
--                          , d4' <- vec2 (_x d4) (- _y d4) =
--    -- the point is somewhere in the center: need to look into all four leafs
--    if (p < _x _rCenter, _y p <= _y _rCenter) of
--        (True , True ) ->




