-----------------------------------------------------------------------------
-- |
-- Module      :  Reactive.Banana.JsHs.ElementHandler
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Reactive.Banana.JsHs.ElementHandler
  ( -- * JS handler
    ElementHandler, elementHandler
    -- * Events
  , pointerEvents, wheelEvents
    -- * Behaviors
  , altKey, ctrlKey, metaKey, shiftKey
  , buttons, downPointers, curPointers, downTime
  ) where



import qualified Control.Event.Handler as RB
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import JsHs.LikeJS.Class
import JsHs.Types
import qualified JsHs.Array as JS
import qualified JsHs.Callback as JS


import Reactive.Banana.JsHs.Types
import qualified Reactive.Banana.JsHs.PointerKeeper as PK



data ElementHandler = ElementHandler
  { state       :: PK.PointerKeeper
  , pointer     :: AddHandler PointerEvent
  , wheel       :: AddHandler Double
  , shiftKeyH   :: AddHandler Bool
  , ctrlKeyH    :: AddHandler Bool
  , altKeyH     :: AddHandler Bool
  , metaKeyH    :: AddHandler Bool
  , buttonsH    :: AddHandler Int
  , downPointsH :: AddHandler (JS.Array PointerPos)
  , curPointsH  :: AddHandler (JS.Array PointerPos)
  , downTimeH   :: AddHandler Double
  }

elementHandler :: JSVal -> IO ElementHandler
elementHandler el = do
  (ahP,      fireP)     <- newAddHandler
  (ahWheel,  fireWheel) <- newAddHandler
  pk <- PK.newPointerKeeper el fireP
  listenToWheel el fireWheel
  return ElementHandler
    { state       = pk
    , pointer     = ahP
    , wheel       = ahWheel
    , shiftKeyH   = RB.mapIO (const $ PK.shiftKey pk) ahP
    , ctrlKeyH    = RB.mapIO (const $ PK.ctrlKey pk) ahP
    , altKeyH     = RB.mapIO (const $ PK.altKey pk) ahP
    , metaKeyH    = RB.mapIO (const $ PK.metaKey pk) ahP
    , buttonsH    = RB.mapIO (const $ PK.buttons pk) ahP
    , downPointsH = RB.mapIO (const $ PK.downPointers pk) ahP
    , curPointsH  = RB.mapIO (const $ PK.curPointers pk) ahP
    , downTimeH   = RB.mapIO (const $ PK.downTime pk) ahP
    }



-- | All pointer events
pointerEvents :: ElementHandler -> MomentIO (Event PointerEvent)
pointerEvents = fromAddHandler . pointer

-- | Mouse wheel up or down (arg is +1 or -1 for scroll up or down accordingly)
wheelEvents :: ElementHandler -> MomentIO (Event Double)
wheelEvents = fromAddHandler . wheel

shiftKey :: ElementHandler -> MomentIO (Behavior Bool)
shiftKey = fromChanges False . shiftKeyH
ctrlKey :: ElementHandler -> MomentIO (Behavior Bool)
ctrlKey = fromChanges False . ctrlKeyH
altKey :: ElementHandler -> MomentIO (Behavior Bool)
altKey = fromChanges False . altKeyH
metaKey :: ElementHandler -> MomentIO (Behavior Bool)
metaKey = fromChanges False . metaKeyH
buttons :: ElementHandler -> MomentIO (Behavior Int)
buttons = fromChanges 0 . buttonsH
downPointers :: ElementHandler -> MomentIO (Behavior (JS.Array PointerPos))
downPointers = fromChanges JS.emptyArray . downPointsH
curPointers :: ElementHandler -> MomentIO (Behavior (JS.Array PointerPos))
curPointers = fromChanges JS.emptyArray . curPointsH
downTime :: ElementHandler -> MomentIO (Behavior Double)
downTime = fromChanges 0 . downTimeH



foreign import javascript unsafe "ReactiveBanana.listenToWheel($1,$2)"
   js_listenToWheel :: JSVal -> JS.Callback (JSVal -> IO ())  -> IO ()
listenToWheel :: JSVal -> (Double -> IO ()) -> IO ()
listenToWheel el fwheel = JS.asyncCallback1 (fwheel . asLikeJS) >>= js_listenToWheel el

