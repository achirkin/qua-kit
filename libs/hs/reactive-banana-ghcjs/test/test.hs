module Main where


import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

--import Data.Coerce
--import Data.Monoid

--import qualified JsHs.JSString as JSString
import JsHs.Types
--import JsHs.Debug
--import JsHs.Callback
--import JsHs.LikeJS.Class
import qualified JsHs.Array as JS
import Reactive.Banana.JsHs
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
    heh <- elementHandler canvas
    network <- compile $ do
      pointerE <- pointerEvents heh
      wheelE   <- wheelEvents heh
      ctrlKeyB <- ctrlKey heh
      curPointersB <- curPointers heh
      reactimate . fmap (pointerC ctx) $ ((\c p ev -> (ev,c,p)) <$> ctrlKeyB <*> curPointersB) <@> pointerE
      reactimate $ wheelCallback canvas <$> wheelE
      return ()
    actuate network
  where
    wheelCallback c delta | delta > 0 = setBGColor c "FFCCCC"
                          | delta < 0 = setBGColor c "CCCCFF"
                          | otherwise = setBGColor c "FFFFFF"
    pointerC :: JSVal -> (PointerEvent, Bool, JS.Array PointerPos) -> IO ()
    pointerC ctx (PointerUp event, _, _) = do
        setStyle ctx "004466"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 2)
                          (posY p - 2)
                          5 5
          )
          $ pointers event
    pointerC ctx (PointerDown event, _, _) = do
        setStyle ctx "FFFFAA"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 7)
                          (posY p - 7)
                          15 15
          )
          $ pointers event
    pointerC ctx (PointerMove _, ctrl, pps) = do
        if ctrl then setStyle ctx "55FF99"
                else setStyle ctx "00FFAA"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 1)
                          (posY p - 1)
                          3 3
          ) pps
    pointerC ctx (PointerCancel event, _, _) = do
        setStyle ctx "FF0000"
        JS.mapIO_
          (\p -> fillRect ctx
                          (posX p - 12)
                          (posY p - 12)
                          25 25
          )
          $ pointers event


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


