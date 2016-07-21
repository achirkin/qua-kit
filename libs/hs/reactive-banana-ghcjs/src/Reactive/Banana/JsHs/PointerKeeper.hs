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
  ( PointerKeeper, newPointerKeeper
  , altKey, ctrlKey, metaKey, shiftKey, modKeys, buttons
  , eventPointerKeeper
  , downTime, downPointers, curPointers
  ) where

import Control.Monad (join)
import JsHs.LikeJS.Class
import JsHs.Types
import qualified JsHs.Array as JS
import qualified JsHs.Callback as JS

import Reactive.Banana.JsHs.Types

newtype PointerKeeper = PointerKeeper JSVal
instance LikeJS "PointerKeeper" PointerKeeper

newPointerKeeper :: JSVal
                 -> (PointerEvent -> IO ())
                 -> IO PointerKeeper
newPointerKeeper el callback = join
     $ js_pointerKeeper el
    <$> m callback PointerUp
    <*> m callback PointerDown
    <*> m callback PointerMove
    <*> m callback PointerCancel
  where
    m f c = JS.asyncCallback1 $ f . c . asLikeJS

foreign import javascript unsafe "$r = new ReactiveBanana.PointerKeeper($1,$2,$3,$4,$5);"
   js_pointerKeeper :: JSVal
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> JS.Callback (JSVal -> IO ())
      -> IO PointerKeeper

foreign import javascript unsafe "$1.altKey"   altKey   :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.ctrlKey"  ctrlKey  :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.metaKey"  metaKey  :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.shiftKey" shiftKey :: PointerKeeper -> IO Bool
foreign import javascript unsafe "$1.buttons"  buttons  :: PointerKeeper -> IO Int
foreign import javascript unsafe "$1.downTime" downTime :: PointerKeeper -> IO Double
foreign import javascript unsafe "$1.downPointers"
    downPointers :: PointerKeeper -> IO (JS.Array PointerPos)
foreign import javascript unsafe "$1.curPointers"
    curPointers :: PointerKeeper -> IO (JS.Array PointerPos)



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

