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
  , play, stop, updateEvents, resizeEvents
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
  , play        :: IO () -- ^ start requestAnimationFrame loop
  , stop        :: IO () -- ^ finish requestAnimationFrame loop
  , render      :: AddHandler Double
  , resizeH     :: AddHandler (Double, Double)
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
  (ahRender, fireRender) <- newAddHandler
  (ahResize, fireResize) <- newAddHandler
  (ahP,      fireP)     <- newAddHandler
  (ahWheel,  fireWheel) <- newAddHandler
  pk <- PK.newPointerKeeper el fireRender fireP (curry fireResize)
  listenToWheel el fireWheel
  return ElementHandler
    { state       = pk
    , play        = PK.play pk
    , stop        = PK.stop pk
    , render      = ahRender
    , resizeH     = ahResize
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



-- | This type of events before rendering animation frame.
--   Event value is a timestamp taken from window.requestAnimationFrame,
--   which is similar to performance.now()
updateEvents :: ElementHandler -> MomentIO (Event Double)
updateEvents = fromAddHandler . render

-- | All pointer events
pointerEvents :: ElementHandler -> MomentIO (Event PointerEvent)
pointerEvents = fromAddHandler . pointer

-- | Mouse wheel up or down (arg is +1 or -1 for scroll up or down accordingly)
wheelEvents :: ElementHandler -> MomentIO (Event Double)
wheelEvents = fromAddHandler . wheel

-- | when element is resized
resizeEvents :: ElementHandler -> MomentIO (Event (Double,Double))
resizeEvents = fromAddHandler . resizeH

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

