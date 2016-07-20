module Main where


import Reactive.Banana.Frameworks

--import Data.Coerce
--import Data.Monoid

--import qualified JsHs.JSString as JSString
import JsHs.Types
--import JsHs.Debug
--import JsHs.Callback
--import JsHs.LikeJS.Class
import qualified JsHs.Array as JS
import Reactive.Banana.JsHs.Pointer
--import Control.Monad (forever)
--import Control.Concurrent (threadDelay)

palette :: JSString -> JSString
palette "pointerdown" = "FFFF00"
palette "pointerup" = "00FFFF"
palette "pointercancel" = "FF0000"
palette "pointermove" = "0055AA"
palette "pointerover" = "AABBFF"
palette "pointerout" = "FF0000"
palette _ = "AAAAAA"

main :: IO ()
main = do
    canvas <- addCanvasToBody
    ctx <- get2dContext canvas
    heh <- htmlElemHandler canvas
    network <- compile $ do
      pointerUpE <- fromAddHandler $ pointerUp heh
      pointerDownE <- fromAddHandler $ pointerDown heh
      pointerMoveE <- fromAddHandler $ pointerMove heh
      pointerCancelE <- fromAddHandler $ pointerCancel heh
      wheelE <- fromAddHandler $ wheel heh
      reactimate $ pointerUpC ctx <$> pointerUpE
      reactimate $ pointerDownC ctx <$> pointerDownE
      reactimate $ pointerMoveC ctx <$> pointerMoveE
      reactimate $ pointerCancelC ctx <$> pointerCancelE
      reactimate $ wheelCallback canvas <$> wheelE
      return ()
    actuate network
  where
    wheelCallback c delta | delta > 0 = setBGColor c "FFCCCC"
                          | delta < 0 = setBGColor c "CCCCFF"
                          | otherwise = setBGColor c "FFFFFF"
    pointerUpC ctx event = do
        setStyle ctx "004466"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 2)
                          (posY p - 2)
                          5 5
          )
          $ pointers event
    pointerDownC ctx event = do
        setStyle ctx "FFFFAA"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 7)
                          (posY p - 7)
                          15 15
          )
          $ pointers event
    pointerMoveC ctx event = do
        setStyle ctx "00FFAA"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 1)
                          (posY p - 1)
                          3 3
          )
          $ pointers event
    pointerCancelC ctx event = do
        setStyle ctx "FF0000"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 12)
                          (posY p - 12)
                          25 25
          )
          $ pointers event


--foreign import javascript unsafe "\
--    \ $1.innerHTML = $2;"
--    setText :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.getContext(\"2d\")"
    get2dContext :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.fillRect($2,$3,$4,$5)"
    fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.fillStyle = '#' + $2;"
    setStyle :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.style.backgroundColor = '#' + $2;"
    setBGColor :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "var body = document.getElementsByTagName(\"body\")[0]; \
    \ body.innerHTML = '<div style=\"width: 1000px; margin: 25px; padding: 18px; \
                      \ background-color: coral; border-color: blue; border-style: solid;\"> \
                      \ <canvas id=\"cvn\" height=\"200\" width=\"200\" style=\"margin: 22px; \
                      \ padding: 10px 20px 30px 25px; border-color: darkred; border-style: dashed; \
                      \ background-color: white; width: 65%; height: 400px;\"></canvas></div>';\
    \ $r = document.getElementById(\"cvn\");"
    addCanvasToBody :: IO JSVal


data HtmlElemHandler = HtmlElemHandler
  { keeper        :: PointerKeeper
  , pointerUp     :: AddHandler PointerEvent
  , pointerDown   :: AddHandler PointerEvent
  , pointerMove   :: AddHandler PointerEvent
  , pointerCancel :: AddHandler PointerEvent
  , wheel         :: AddHandler Double
  }

htmlElemHandler :: JSVal -> IO HtmlElemHandler
htmlElemHandler el = do
  (ahUp,     fireUp)     <- newAddHandler
  (ahDown,   fireDown)   <- newAddHandler
  (ahMove,   fireMove)   <- newAddHandler
  (ahCancel, fireCancel) <- newAddHandler
  (ahWheel,  fireWheel)  <- newAddHandler
  pk <- pointerKeeper el fireUp fireDown fireMove fireCancel
  listenToWheel el fireWheel
  return HtmlElemHandler
    { keeper        = pk
    , pointerUp     = ahUp
    , pointerDown   = ahDown
    , pointerMove   = ahMove
    , pointerCancel = ahCancel
    , wheel         = ahWheel
    }

