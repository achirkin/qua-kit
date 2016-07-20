{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reactive.Banana.JsHs.Pointer
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Reactive.Banana.JsHs.Pointer
--    ( PointerEvent
----    , pointerId, width, height, pressure, tiltX, tiltY, pointerType, isPrimary, evType
----    , clientX, clientY, pageX, pageY, screenX, screenY
--    ) where
  where

import Control.Monad (join)
import JsHs.LikeJS.Class
import JsHs.Types
import qualified JsHs.Array as JS
import qualified JsHs.Callback as JS
import Data.Typeable (Typeable)

-- | Callback for all pointer events
type PointerEventCallback = PointerEvent -> IO ()
-- | Callback for mouse wheel event (arg is +1 or -1 for scroll up or down accordingly)
type WheelEventCallback = Double -> IO ()

newtype PointerKeeper = PointerKeeper JSVal deriving Typeable
instance LikeJS "PointerKeeper" PointerKeeper

listenToWheel :: JSVal -> WheelEventCallback -> IO ()
listenToWheel el fwheel = JS.asyncCallback1 (fwheel . asLikeJS) >>= js_listenToWheel el

pointerKeeper :: JSVal
              -> PointerEventCallback
              -> PointerEventCallback
              -> PointerEventCallback
              -> PointerEventCallback
              -> IO PointerKeeper
pointerKeeper el fup fdown fmove fcancel = join
     $ js_pointerKeeper el
    <$> m fup
    <*> m fdown
    <*> m fmove
    <*> m fcancel
  where
    m f = JS.asyncCallback1 $ f . asLikeJS

foreign import javascript unsafe "ReactiveBanana.listenToWheel($1,$2)"
   js_listenToWheel :: JSVal
                    -> JS.Callback (JSVal -> IO ())
                    -> IO ()

{-# INLINE js_pointerKeeper #-}
foreign import javascript unsafe "$r = new ReactiveBanana.PointerKeeper($1,$2,$3,$4,$5);"
   js_pointerKeeper :: JSVal
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> IO PointerKeeper

-- | Use JavaScript ReactiveBanana.PointerEvent
newtype PointerEvent = PointerEvent JSVal deriving Typeable
instance LikeJS "PointerEvent" PointerEvent

data PointerEventType
  = PointerUp
  | PointerDown
  | PointerMove
  | PointerCancel
  deriving (Eq,Show,Read,Ord,Bounded)

instance Enum PointerEventType where
  toEnum 0 = PointerUp
  toEnum 1 = PointerDown
  toEnum 2 = PointerMove
  toEnum 3 = PointerCancel
  toEnum x = error $ "Unknown PointerEventType " ++ show x ++ " (only values 0-3 are possible)."
  fromEnum PointerUp = 0
  fromEnum PointerDown = 1
  fromEnum PointerMove = 2
  fromEnum PointerCancel = 3

-- | A modifier key. A list of modifier keys is reported for every key press
-- and mouse click.
data ModKey = Shift | Ctrl | Alt | Meta
  deriving (Show, Read, Ord, Eq, Bounded, Enum)

-- | A list of all modifier keys
enumerateModKeys :: [ModKey]
enumerateModKeys = [Shift .. Meta]

modKeys :: PointerEvent -> [ModKey]
modKeys ev = map snd
           . filter fst
           $ zip [shiftKey ev, ctrlKey ev, altKey ev, metaKey ev] enumerateModKeys

{-# INLINE altKey #-}
foreign import javascript unsafe "$1.altKey" altKey :: PointerEvent -> Bool
{-# INLINE ctrlKey #-}
foreign import javascript unsafe "$1.ctrlKey" ctrlKey :: PointerEvent -> Bool
{-# INLINE metaKey #-}
foreign import javascript unsafe "$1.metaKey" metaKey :: PointerEvent -> Bool
{-# INLINE shiftKey #-}
foreign import javascript unsafe "$1.shiftKey" shiftKey :: PointerEvent -> Bool

-- | Javascript object containing x and y screen coordinates
newtype HTMLPos = HTMLPos JSVal deriving Typeable
instance LikeJS "Object" HTMLPos

{-# INLINE posX #-}
foreign import javascript unsafe "$1.x" posX :: HTMLPos -> Double
{-# INLINE posY #-}
foreign import javascript unsafe "$1.y" posY :: HTMLPos -> Double


{-# INLINE pointers #-}
foreign import javascript unsafe "$1.pointers" pointers :: PointerEvent -> JS.Array HTMLPos

{-# INLINE js_pointerEventType #-}
foreign import javascript unsafe "$1.pointerEventType" js_pointerEventType :: PointerEvent -> Int
pointerEventType :: PointerEvent -> PointerEventType
pointerEventType = toEnum . js_pointerEventType

{-# INLINE pointerJSEventType #-}
foreign import javascript unsafe "$1.type" pointerJSEventType :: PointerEvent -> JSString


{-# INLINE button #-}
foreign import javascript unsafe "$1.button" button :: PointerEvent -> Int
{-# INLINE buttons #-}
foreign import javascript unsafe "$1.buttons" buttons :: PointerEvent -> Int

{-# INLINE eventTarget #-}
foreign import javascript unsafe "$1.target" eventTarget :: PointerEvent -> JSVal

--{-# INLINE pointerId #-}
--foreign import javascript unsafe "$1[\"pointerId\"] ? $1[\"pointerId\"] : 0"
--  pointerId :: PointerEvent -> Int
--
--{-# INLINE width #-}
--foreign import javascript unsafe "$1[\"width\"] ? $1[\"width\"] : 0"
--  width :: PointerEvent -> Double
--
--{-# INLINE height #-}
--foreign import javascript unsafe "$1[\"height\"] ? $1[\"height\"] : 0"
--  height :: PointerEvent -> Double
--
--{-# INLINE pressure #-}
--foreign import javascript unsafe "$1[\"pressure\"] ? $1[\"pressure\"] : 0"
--  pressure :: PointerEvent -> Double
--
--{-# INLINE tiltX #-}
--foreign import javascript unsafe "$1[\"tiltX\"] ? $1[\"tiltX\"] : 0"
--  tiltX :: PointerEvent -> Int
--
--{-# INLINE tiltY #-}
--foreign import javascript unsafe "$1[\"tiltY\"] ? $1[\"tiltY\"] : 0"
--  tiltY :: PointerEvent -> Int
--
--{-# INLINE pointerType #-}
--foreign import javascript unsafe "$1[\"pointerType\"] ? $1[\"pointerType\"] : \"\""
--  pointerType :: PointerEvent -> JSString
--
--{-# INLINE isPrimary #-}
--foreign import javascript unsafe "$1[\"isPrimary\"] ? $1[\"isPrimary\"] : false"
--  isPrimary :: PointerEvent -> Bool
--
--{-# INLINE evType #-}
--foreign import javascript unsafe "$1[\"type\"] ? $1[\"type\"] : null"
--  evType :: PointerEvent -> JSString
--
--
--{-# INLINE clientX #-}
--foreign import javascript unsafe "$1[\"clientX\"]"
--  clientX :: PointerEvent -> Double
--{-# INLINE clientY #-}
--foreign import javascript unsafe "$1[\"clientY\"]"
--  clientY :: PointerEvent -> Double
--{-# INLINE pageX #-}
--foreign import javascript unsafe "$1[\"pageX\"]"
--  pageX :: PointerEvent -> Double
--{-# INLINE pageY #-}
--foreign import javascript unsafe "$1[\"pageY\"]"
--  pageY :: PointerEvent -> Double
--{-# INLINE screenX #-}
--foreign import javascript unsafe "$1[\"screenX\"]"
--  screenX :: PointerEvent -> Double
--{-# INLINE screenY #-}
--foreign import javascript unsafe "$1[\"screenY\"]"
--  screenY :: PointerEvent -> Double


