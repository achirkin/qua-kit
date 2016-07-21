{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reactive.Banana.JsHs.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Reactive.Banana.JsHs.Types
  ( -- * Pointer events
    PointerEvent (..)
  , PointerEventValue (..), pointers, eventType, button
    -- * Modifiers
  , ModKey (..)
    -- * Positions
  , PointerPos, posX, posY, unpackPos
  ) where



import qualified JsHs.Array as JS
import JsHs.Types
import JsHs.LikeJS.Class

-- | Unified representation for MouseEvent and TouchEvent in JS
data PointerEvent
  = PointerUp PointerEventValue
  | PointerDown PointerEventValue
  | PointerMove PointerEventValue
  | PointerCancel PointerEventValue

-- | Use JavaScript ReactiveBanana.PointerEventValue
newtype PointerEventValue = PointerEventValue JSVal
instance LikeJS "PointerEvent" PointerEventValue

-- | Positions of all pointers
foreign import javascript unsafe "$1.pointers" pointers  :: PointerEventValue -> JS.Array PointerPos
-- | JS type of event
foreign import javascript unsafe "$1.type"     eventType :: PointerEventValue -> JSString
-- | Id of a mouse button pressed (or zero for touches)
foreign import javascript unsafe "$1.button"   button    :: PointerEventValue -> Int


-- | A modifier key. A list of modifier keys is reported for every key press
-- and mouse click.
data ModKey = Shift | Ctrl | Alt | Meta
  deriving (Show, Read, Ord, Eq, Bounded, Enum)


-- | Javascript object containing x and y screen coordinates
newtype PointerPos = PointerPos JSVal
instance LikeJS "Object" PointerPos

-- | Get pointer x coordinate
foreign import javascript unsafe "$1.x" posX :: PointerPos -> Double
-- | Get pointer y coordinate
foreign import javascript unsafe "$1.y" posY :: PointerPos -> Double
-- | Get pointer coordinates
foreign import javascript unsafe "$r1=$1.x;$r2=$1.y;" unpackPos :: PointerPos -> (Double, Double)
