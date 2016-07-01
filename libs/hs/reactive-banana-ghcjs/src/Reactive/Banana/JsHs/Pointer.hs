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
    ( Pointer
    , PointerEvent
    , pointerId, width, height, pressure, tiltX, tiltY, pointerType, isPrimary, evType
    , clientX, clientY, pageX, pageY, screenX, screenY
    ) where

import JsHs.LikeJS.Class
import JsHs.Types
import Data.Typeable (Typeable)

-- | JavaSctipt object representing any mouse or finger interaction
newtype Pointer = Pointer JSVal deriving Typeable
instance LikeJS "Pointer" Pointer

-- | Use JavaScript PointerEvent
--   https://www.w3.org/TR/pointerevents/#pointerevent-interface
--
-- WebIDL
--dictionary PointerEventInit : MouseEventInit {
--    long      pointerId = 0;
--    double    width = 0;
--    double    height = 0;
--    float     pressure = 0;
--    long      tiltX = 0;
--    long      tiltY = 0;
--    DOMString pointerType = "";
--    boolean   isPrimary = false;
--};
--
--[Constructor(DOMString type, optional PointerEventInit eventInitDict)]
--interface PointerEvent : MouseEvent {
--    readonly    attribute long      pointerId;
--    readonly    attribute double    width;
--    readonly    attribute double    height;
--    readonly    attribute float     pressure;
--    readonly    attribute long      tiltX;
--    readonly    attribute long      tiltY;
--    readonly    attribute DOMString pointerType;
--    readonly    attribute boolean   isPrimary;
--};
-- Also using polyfills from https://github.com/jquery/PEP
newtype PointerEvent = PointerEvent JSVal deriving Typeable
instance LikeJS "PointerEvent" PointerEvent


{-# INLINE pointerId #-}
foreign import javascript unsafe "$1[\"pointerId\"] ? $1[\"pointerId\"] : 0"
  pointerId :: PointerEvent -> Int

{-# INLINE width #-}
foreign import javascript unsafe "$1[\"width\"] ? $1[\"width\"] : 0"
  width :: PointerEvent -> Double

{-# INLINE height #-}
foreign import javascript unsafe "$1[\"height\"] ? $1[\"height\"] : 0"
  height :: PointerEvent -> Double

{-# INLINE pressure #-}
foreign import javascript unsafe "$1[\"pressure\"] ? $1[\"pressure\"] : 0"
  pressure :: PointerEvent -> Double

{-# INLINE tiltX #-}
foreign import javascript unsafe "$1[\"tiltX\"] ? $1[\"tiltX\"] : 0"
  tiltX :: PointerEvent -> Int

{-# INLINE tiltY #-}
foreign import javascript unsafe "$1[\"tiltY\"] ? $1[\"tiltY\"] : 0"
  tiltY :: PointerEvent -> Int

{-# INLINE pointerType #-}
foreign import javascript unsafe "$1[\"pointerType\"] ? $1[\"pointerType\"] : \"\""
  pointerType :: PointerEvent -> JSString

{-# INLINE isPrimary #-}
foreign import javascript unsafe "$1[\"isPrimary\"] ? $1[\"isPrimary\"] : false"
  isPrimary :: PointerEvent -> Bool

{-# INLINE evType #-}
foreign import javascript unsafe "$1[\"type\"] ? $1[\"type\"] : null"
  evType :: PointerEvent -> JSString


{-# INLINE clientX #-}
foreign import javascript unsafe "$1[\"clientX\"]"
  clientX :: PointerEvent -> Double
{-# INLINE clientY #-}
foreign import javascript unsafe "$1[\"clientY\"]"
  clientY :: PointerEvent -> Double
{-# INLINE pageX #-}
foreign import javascript unsafe "$1[\"pageX\"]"
  pageX :: PointerEvent -> Double
{-# INLINE pageY #-}
foreign import javascript unsafe "$1[\"pageY\"]"
  pageY :: PointerEvent -> Double
{-# INLINE screenX #-}
foreign import javascript unsafe "$1[\"screenX\"]"
  screenX :: PointerEvent -> Double
{-# INLINE screenY #-}
foreign import javascript unsafe "$1[\"screenY\"]"
  screenY :: PointerEvent -> Double


