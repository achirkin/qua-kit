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



{-# INLINE altKey #-}
foreign import javascript unsafe "$1.altKey" altKey :: PointerKeeper -> Bool
{-# INLINE ctrlKey #-}
foreign import javascript unsafe "$1.ctrlKey" ctrlKey :: PointerKeeper -> Bool
{-# INLINE metaKey #-}
foreign import javascript unsafe "$1.metaKey" metaKey :: PointerKeeper -> Bool
{-# INLINE shiftKey #-}
foreign import javascript unsafe "$1.shiftKey" shiftKey :: PointerKeeper -> Bool





-- | Use JavaScript ReactiveBanana.PointerEvent
newtype PointerEvent = PointerEvent JSVal deriving Typeable
instance LikeJS "PointerEvent" PointerEvent

{-# INLINE pointers #-}
foreign import javascript unsafe "$1.pointers" pointers :: PointerEvent -> JS.Array PointerPos
{-# INLINE js_pointerEventType #-}
foreign import javascript unsafe "$1.pointerEventType" js_pointerEventType :: PointerEvent -> Int
pointerEventType :: PointerEvent -> PointerEventType
pointerEventType = toEnum . js_pointerEventType
{-# INLINE pointerJSEventType #-}
foreign import javascript unsafe "$1.type" pointerJSEventType :: PointerEvent -> JSString
{-# INLINE button #-}
foreign import javascript unsafe "$1.button" button :: PointerEvent -> Int
{-# INLINE buttons #-}
foreign import javascript unsafe "$1.buttons" buttons :: PointerKeeper -> Int

{-# INLINE eventTarget #-}
foreign import javascript unsafe "$1.target" eventTarget :: PointerEvent -> JSVal
{-# INLINE eventPointer #-}
foreign import javascript unsafe "$1.target.pointerKeeper" eventPointer :: PointerEvent -> PointerKeeper





-- | Javascript object containing x and y screen coordinates
newtype PointerPos = PointerPos JSVal deriving Typeable
instance LikeJS "Object" PointerPos

{-# INLINE posX #-}
foreign import javascript unsafe "$1.x" posX :: PointerPos -> Double
{-# INLINE posY #-}
foreign import javascript unsafe "$1.y" posY :: PointerPos -> Double




-- | Type of a pointer event action
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

-- | List event's active modifier keys
modKeys :: PointerKeeper -> [ModKey]
modKeys ev = map snd
           . filter fst
           $ zip [shiftKey ev, ctrlKey ev, altKey ev, metaKey ev] enumerateModKeys
