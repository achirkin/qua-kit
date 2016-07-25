{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( Time (..), getTime
  , HTMLElement (..)
    -- * Events
  , PointerEvent (..)
  , PointerEventValue (..), pointers, eventType, button
  , ResizeEvent (..)
  , WheelEvent (..)
  , ElementClick (..)
    -- * Modifiers
  , ModKey (..)
    -- * Positions
  , Coords2D, coordX, coordY, unpackCoords2D
  ) where



import qualified JsHs.Array as JS
import JsHs.Types
import JsHs.LikeJS.Class

newtype HTMLElement = HTMLElement JSVal
instance LikeJS "HTMLElement" HTMLElement


-- | Current time in milliseconds
foreign import javascript unsafe "performance.now()"
    getTime :: IO Time


-- | Unified representation for MouseEvent and TouchEvent in JS
data PointerEvent
  = PointerUp PointerEventValue
  | PointerDown PointerEventValue
  | PointerMove PointerEventValue
  | PointerCancel PointerEventValue
  | PointerClick PointerEventValue

-- | Mouse wheel event
data WheelEvent = WheelUp | WheelDown
  deriving (Eq,Show,Ord)
instance LikeJS "Number" WheelEvent where
  asJSVal WheelUp = asJSVal (1 :: Double)
  asJSVal WheelDown = asJSVal (-1 :: Double)
  asLikeJS jsv = case asLikeJS jsv :: Double of
                   v -> if v >= 0 then WheelUp else WheelDown

newtype ResizeEvent = ResizeEvent Coords2D
instance LikeJS "Array" ResizeEvent

-- | Use JavaScript ReactiveBanana.PointerEventValue
newtype PointerEventValue = PointerEventValue JSVal
instance LikeJS "PointerEvent" PointerEventValue

-- | Click on an element
newtype ElementClick = ElementClick HTMLElement
instance LikeJS "HTMLElement" ElementClick

-- | Time of events is Double
newtype Time = Time Double
  deriving (Eq,Ord,Show,Enum,Num,Real,RealFrac,RealFloat,Fractional,Floating)
instance LikeJS "Number" Time where
  asLikeJS = Time . asLikeJS
  asJSVal (Time v) = asJSVal v

-- | Positions of all pointers
foreign import javascript unsafe "$1.pointers" pointers  :: PointerEventValue -> JS.Array Coords2D
-- | JS type of event
foreign import javascript unsafe "$1.type"     eventType :: PointerEventValue -> JSString
-- | Id of a mouse button pressed (or zero for touches)
foreign import javascript unsafe "$1.button"   button    :: PointerEventValue -> Int


-- | A modifier key. A list of modifier keys is reported for every key press
-- and mouse click.
data ModKey = Shift | Ctrl | Alt | Meta
  deriving (Show, Read, Ord, Eq, Bounded, Enum)


-- | Javascript object containing x and y screen coordinates
newtype Coords2D = Coords2D JSVal
instance LikeJS "Array" Coords2D

-- | Get pointer x coordinate
foreign import javascript unsafe "$1[0]" coordX :: Coords2D -> Double
-- | Get pointer y coordinate
foreign import javascript unsafe "$1[1]" coordY :: Coords2D -> Double
-- | Get pointer coordinates
foreign import javascript unsafe "$r1=$1.x;$r2=$1.y;" unpackCoords2D :: Coords2D -> (Double, Double)
