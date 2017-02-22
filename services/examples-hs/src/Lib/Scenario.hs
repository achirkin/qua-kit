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
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Lib.Scenario
  ( -- * Data types
    Block (..), bCenter, bWalls, bBound, content
  , Wall (..), wCenter, wOrientation
  , Scenario
  , fromList, toList
  ) where

-- import Data.Functor.Identity
-- import Data.Functor.Const
-- import           Data.IntMap.Strict          (IntMap)
-- import qualified Data.IntMap.Strict          as IntMap
-- import           Data.Maybe                  (fromMaybe, isJust)
-- import Data.Proxy
-- import GHC.TypeLits
-- import Unsafe.Coerce
import qualified Control.Lens                as Lens
import           Data.Semigroup
import           GHC.Exts                    (IsList (..))
--import Control.Lens.Operators ((%%=), (%=))
-- import qualified Control.Parallel.Strategies as PS

import           Numeric.EasyTensor

-- import           Lib.Constants
import           Lib.ExtraTypes
import           Lib.Region

-- | A building block is defined by its center and a list of walls
data Block a = Block
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
  , _content :: !a
    -- ^ Any data stored together with a block
  }
  deriving Show

instance Functor Block where
  fmap f b = b{_content = f (_content b)}

instance BoundedBy Vec2f (Block a) where
  bounds = _bBound
  {-# INLINE bounds #-}

instance Spatial 2 Float (Block a) where
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
  distL2SquaredTo p Block{..} = case parse (0, dot p' p') _bWalls of
                                   (n, d2) -> if mod n 2 == (1 :: Int) then 0 else d2
    where
      p' = p - _bCenter
      ray = vec2 1 0
      intersects (Wall c o) | a <- c - o - p'
                            , b <- c + o - p'
                            , ra <- det2 ray a >= 0
                            , rb <- det2 ray b >= 0
                            , ab <- det2 a b > 0 = if (ra /= rb) && (ra /= ab) then 1 else 0
      parse (n, d2) (w:ws) = case (n + intersects w, min d2 $ distL2SquaredTo p' w) of r -> r `seq` parse r ws
      parse r [] = r


-- | A single wall of a building is represented by a relative (w.r.t. building center) position
--   and orientation.
--   To the left of the orientation vector is always a solid - building block.
--   To the right of the orientation vector is always a void - empty space.
--   The width of a wall is two times the width of the orientation vector.
--   The position of a wall is a position of its center.
--   A wall has no width parameter.
data Wall = Wall
  { _wCenter      :: !Vec2f
    -- ^ Position of a center of a wall relative to position of a parent object.
  , _wOrientation :: !Vec2f
    -- ^ Orientation of a wall.
    --   Direction of this vector represents CCW traverse of building walls.
    --   Size of this vector is a half of the wall width.
  }
  deriving Show


instance BoundedBy Vec2f Wall where
  bounds Wall {..} = let o = abs _wOrientation in MinMax (_wCenter - o) (_wCenter + o)
  {-# INLINE bounds #-}


instance Spatial 2 Float Wall where
  isInside _ _ = False
  distL2SquaredTo p Wall{..} = if between then d2 else min lu2 lv2
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




-- | All building blocks
type Scenario a = Region (Block a)

Lens.makeLenses ''Block
Lens.makeLenses ''Wall
