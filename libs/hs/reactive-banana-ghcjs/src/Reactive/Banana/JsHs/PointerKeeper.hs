{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reactive.Banana.JsHs.PointerKeeper
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Reactive.Banana.JsHs.PointerKeeper
  ( PointerKeeper, newPointerKeeper, listenToWheel, elementOnClick
  , altKey, ctrlKey, metaKey, shiftKey, modKeys, buttons
  , eventPointerKeeper
  , downTime, downPointers, curPointers
  , play, stop
  ) where

import Control.Monad (join)
import JsHs.LikeJS.Class
import JsHs.Types
import qualified JsHs.Array as JS
import qualified JsHs.Callback as JS

import Reactive.Banana.JsHs.Types

newtype PointerKeeper = PointerKeeper JSVal
instance LikeJS "PointerKeeper" PointerKeeper

newPointerKeeper :: HTMLElement
                 -> (Time -> IO ())
                 -> (PointerEvent -> IO ())
                 -> (ResizeEvent -> IO ())
                 -> IO PointerKeeper
newPointerKeeper el updateCallback callback resizeCallback = join
     $ js_pointerKeeper el
    <$> JS.asyncCallback1 (updateCallback . asLikeJS)
    <*> m callback PointerUp
    <*> m callback PointerClick
    <*> m callback PointerDown
    <*> m callback PointerMove
    <*> m callback PointerCancel
    <*> JS.asyncCallback1 (resizeCallback . asLikeJS)
  where
    m f c = JS.asyncCallback1 $ f . c . asLikeJS

foreign import javascript unsafe "$r = new ReactiveBanana.PointerKeeper($1,$2,$3,$4,$5,$6,$7,$8);"
   js_pointerKeeper :: HTMLElement
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> IO PointerKeeper


foreign import javascript unsafe "$1.play()" play :: PointerKeeper -> IO ()
foreign import javascript unsafe "$1.stop()" stop :: PointerKeeper -> IO ()
foreign import javascript unsafe "$1.altKey"   altKey   :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.ctrlKey"  ctrlKey  :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.metaKey"  metaKey  :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.shiftKey" shiftKey :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1['buttons']"  buttons  :: PointerKeeper -> IO Int
foreign import javascript unsafe "$1.downTime" downTime :: PointerKeeper -> IO Time
foreign import javascript unsafe "$1.downPointers"
    downPointers :: PointerKeeper -> IO (JS.Array Coords2D)
foreign import javascript unsafe "$1.curPointers"
    curPointers :: PointerKeeper -> IO (JS.Array Coords2D)



-- | List event's active modifier keys
{-# INLINE modKeys #-}
modKeys :: PointerKeeper -> IO [ModKey]
modKeys ev = f metaKey Meta []
         >>= f altKey Alt
         >>= f ctrlKey Ctrl
         >>= f shiftKey Shift
  where
    f m val s = (\r -> if r then val:s else s) <$> m ev

foreign import javascript unsafe "$1.target.pointerKeeper"
  eventPointerKeeper :: PointerEventValue -> PointerKeeper


foreign import javascript unsafe "$1.listenToWheel($2)"
   js_listenToWheel :: PointerKeeper -> JS.Callback (JSVal -> IO ())  -> IO ()
listenToWheel :: PointerKeeper -> (WheelEvent -> IO ()) -> IO ()
listenToWheel el fwheel = JS.asyncCallback1 (fwheel . asLikeJS) >>= js_listenToWheel el


-- | Simple event when JSElement is clicked
elementOnClick :: HTMLElement -> (ElementClick -> IO ()) -> IO ()
elementOnClick element clickFun = do
    clickCallBack <- JS.asyncCallback (clickFun $ ElementClick element)
    elementOnClick' element clickCallBack
foreign import javascript unsafe "\
    \ $1.addEventListener('click', function(event){ \
    \     var e = window.event || event; \
    \     e.preventDefault(); \
    \     e.stopPropagation(); \
    \     $2(); \
    \     return false; \
    \ });"
    elementOnClick' :: HTMLElement -> JS.Callback (IO ()) -> IO ()
