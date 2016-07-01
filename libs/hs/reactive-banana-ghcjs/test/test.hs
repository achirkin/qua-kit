module Main where

import Data.Coerce
import Data.Monoid

import qualified JsHs.JSString as JSString
import JsHs.Types
import JsHs.Debug
import JsHs.Callback
import JsHs.LikeJS.Class
import Reactive.Banana.JsHs.Pointer

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
  canvas <- addCanvasToBody 500 500
  disableContextMenu canvas
  disableDefaultTouchActions canvas
  ctx <- get2dContext canvas
--  printJSVal canvas
  callback <- coerce . asyncCallback1 $ \jsev -> do
      let event = asLikeJS jsev :: PointerEvent
      if pressure event == 0
      then setStyle ctx "CCCCCC"
      else setStyle ctx (palette $ evType event)
      fillRect ctx (round $ pageX event)
                   (round $ pageY event)
                   (min 100 . max 2 . round $ 5 + 5 * pressure event)
                   (min 100 . max 2 . round $ 5 + 5 * pressure event)
--      setText canvas $ evType event <> " event"
--        <> "<br>\npointerId: " <> JSString.pack (show $ pointerId event)
--        <> "<br>\nwidth: " <> JSString.pack (show $ width event)
--        <> "<br>\nheight: " <> JSString.pack (show $ height event)
--        <> "<br>\npressure: " <> JSString.pack (show $ pressure event)
--        <> "<br>\ntiltX: " <> JSString.pack (show $ tiltX event)
--        <> "<br>\ntiltY: " <> JSString.pack (show $ tiltY event)
--        <> "<br>\npointerType: " <> pointerType event
--        <> "<br>\nisPrimary: " <> JSString.pack (show $ isPrimary event)
      printJSVal jsev
      return ()
  wheelCallback <- coerce . asyncCallback1 $ \jsev -> do
      let val = asLikeJS jsev :: Double
--      setText canvas $ "wheel event: "  <> JSString.pack (show val)
      printJSVal jsev
      return ()
  addEvent canvas "pointerdown" callback
  addEvent canvas "pointerup" callback
  addEvent canvas "pointercancel" callback
  addEvent canvas "pointermove" callback
  addEvent canvas "pointerover" callback
  addEvent canvas "pointerout" callback
  onMouseWheel canvas wheelCallback
  putStrLn "Hello world!"


foreign import javascript safe "var ca = document.createElement('canvas'); ca.width = $1; ca.height = $2; document.body.appendChild(ca); $r = ca;"
    addCanvasToBody :: Int -> Int -> IO JSVal


foreign import javascript safe "$1.addEventListener($2, $3, false);"
    addEvent:: JSVal -> JSString -> Callback (PointerEvent -> IO ()) -> IO ()


foreign import javascript unsafe
  "$1['style']['touch-action'] = \"none\";"
    disableDefaultTouchActions :: JSVal -> IO ()

foreign import javascript unsafe
  "$1.addEventListener('contextmenu',function(e){e.preventDefault();e.stopPropagation();return false;});"
    disableContextMenu :: JSVal -> IO ()


foreign import javascript unsafe "\
    \ $1.addEventListener('wheel', function(event){ \
    \     var e = window.event || event; \
    \     e.preventDefault(); \
    \     e.stopPropagation(); \
    \     $2(e['wheelDelta'] > 0 || e['detail'] < 0 || e['deltaY'] < 0 ? (1.0) : (-1.0)); \
    \     return false; \
    \ });"
    onMouseWheel :: JSVal -> Callback (Double -> IO ()) -> IO ()


foreign import javascript unsafe "\
    \ $1.innerHTML = $2;"
    setText :: JSVal -> JSString -> IO ()


foreign import javascript unsafe "$1.getContext(\"2d\")"
    get2dContext :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.fillRect($2,$3,$4,$5)"
    fillRect :: JSVal -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.fillStyle = '#' + $2;"
    setStyle :: JSVal -> JSString -> IO ()

--ctx.fillStyle="#FF0000";
--var c=document.getElementById("myCanvas");
--var ctx=c.getContext("2d");
--ctx.fillRect(20,20,150,100);
