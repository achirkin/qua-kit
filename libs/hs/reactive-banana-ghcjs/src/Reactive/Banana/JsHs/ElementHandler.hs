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
    HTMLClickHandler, clickHandler, clickEvents
  , ElementHandler, elementHandler
  , play, stop, updateEvents, resizeEvents
    -- * Events
  , pointerEvents, wheelEvents
    -- * Behaviors
  , altKey, ctrlKey, metaKey, shiftKey
  , buttons, downPointers, curPointers, downTime
  , viewPortSize
  ) where



import qualified Control.Event.Handler as RB
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
--import JsHs.LikeJS.Class
--import JsHs.Types
import qualified JsHs.Array as JS
--import qualified JsHs.Callback as JS


import Reactive.Banana.JsHs.Types
import qualified Reactive.Banana.JsHs.PointerKeeper as PK

-- | Handler for a mouse click event
newtype HTMLClickHandler = HTMLClickHandler
  { clickH :: AddHandler ElementClick
  }

clickHandler :: HTMLElement -> IO HTMLClickHandler
clickHandler el = do
  (ah, fire) <- newAddHandler
  PK.elementOnClick el fire
  return $ HTMLClickHandler ah

-- | Triggers on HTMLElement click
clickEvents :: HTMLClickHandler -> MomentIO (Event ElementClick)
clickEvents = fromAddHandler . clickH


data ElementHandler = ElementHandler
  { state       :: PK.PointerKeeper
  , play        :: IO () -- ^ start requestAnimationFrame loop
  , stop        :: IO () -- ^ finish requestAnimationFrame loop
  , render      :: AddHandler Time
  , resizeH     :: AddHandler ResizeEvent
  , pointer     :: AddHandler PointerEvent
  , wheel       :: AddHandler WheelEvent
  , shiftKeyH   :: AddHandler Bool
  , ctrlKeyH    :: AddHandler Bool
  , altKeyH     :: AddHandler Bool
  , metaKeyH    :: AddHandler Bool
  , buttonsH    :: AddHandler Int
  , downPointsH :: AddHandler (JS.Array Coords2D)
  , curPointsH  :: AddHandler (JS.Array Coords2D)
  , downTimeH   :: AddHandler Time
  }

elementHandler :: HTMLElement -> IO ElementHandler
elementHandler el = do
  (ahRender, fireRender) <- newAddHandler
  (ahResize, fireResize) <- newAddHandler
  (ahP,      fireP)     <- newAddHandler
  (ahWheel,  fireWheel) <- newAddHandler
  pk <- PK.newPointerKeeper el fireRender fireP fireResize
  PK.listenToWheel pk fireWheel
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
updateEvents :: ElementHandler -> MomentIO (Event Time)
updateEvents = fromAddHandler . render

-- | All pointer events
pointerEvents :: ElementHandler -> MomentIO (Event PointerEvent)
pointerEvents = fromAddHandler . pointer

-- | Mouse wheel up or down (arg is +1 or -1 for scroll up or down accordingly)
wheelEvents :: ElementHandler -> MomentIO (Event WheelEvent)
wheelEvents = fromAddHandler . wheel

-- | when element is resized
resizeEvents :: ElementHandler -> MomentIO (Event ResizeEvent)
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
downPointers :: ElementHandler -> MomentIO (Behavior (JS.Array Coords2D))
downPointers = fromChanges JS.emptyArray . downPointsH
curPointers :: ElementHandler -> MomentIO (Behavior (JS.Array Coords2D))
curPointers = fromChanges JS.emptyArray . curPointsH
downTime :: ElementHandler -> MomentIO (Behavior Time)
downTime = fromChanges 0 . downTimeH
-- | when element is resized
viewPortSize :: ElementHandler -> MomentIO (Behavior Coords2D)
viewPortSize eh = do
  isize <- liftIO $ PK.viewPortSize (state eh)
  fromChanges isize $ (\(ResizeEvent s) -> s) <$> resizeH eh
