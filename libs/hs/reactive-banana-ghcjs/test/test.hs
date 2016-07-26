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
    myButton <- getButton
    ctx <- get2dContext canvas
    heh <- elementHandler canvas
    bh <- clickHandler myButton
    network <- compile $ do
      clickE <- clickEvents bh

      pointerE <- pointerEvents heh
      wheelE   <- wheelEvents heh
--      updateE <- updateEvents heh
      ctrlKeyB <- ctrlKey heh
      curPointersB <- curPointers heh
      buttonsB <- buttons heh
      reactimate . fmap (pointerC ctx)
                 $ ((\c b p ev -> (ev,(c, b),p)) <$> ctrlKeyB <*> buttonsB <*> curPointersB)
                <@> pointerE
      reactimate $ wheelCallback canvas <$> wheelE
      reactimate $ (\_ -> putStrLn "click!") <$> clickE
      return ()
    actuate network
    play heh
  where
    wheelCallback c WheelUp = setBGColor c "FFCCCC"
    wheelCallback c WheelDown = setBGColor c "CCCCFF"
    pointerC :: JSVal -> (PointerEvent, (Bool, Int), JS.Array Coords2D) -> IO ()
    pointerC ctx (PointerClick _, _, pps) = do
        setStyle ctx "00AA66"
        JS.mapIO_
          (\p -> fillRect ctx
                          (coordX p - 4)
                          (coordY p - 4)
                          9 9
          ) pps
    pointerC ctx (PointerUp event, _, _) = do
        setStyle ctx "004466"
        JS.mapIO_
          (\p -> fillRect ctx
                          (coordX p - 2)
                          (coordY p - 2)
                          5 5
          )
          $ pointers event
    pointerC ctx (PointerDown event, _, _) = do
        setStyle ctx "FFFFAA"
        JS.mapIO_
          (\p -> fillRect ctx
                          (coordX p - 7)
                          (coordY p - 7)
                          15 15
          )
          $ pointers event
    pointerC ctx (PointerMove _, (ctrl, _), pps) = do
        if ctrl then setStyle ctx "55FF99"
                else setStyle ctx "00FFAA"
        JS.mapIO_
          (\p -> fillRect ctx
                          (coordX p - 1)
                          (coordY p - 1)
                          3 3
          ) pps
    pointerC ctx (PointerCancel event, _, _) = do
        setStyle ctx "FF0000"
        JS.mapIO_
          (\p -> fillRect ctx
                          (coordX p - 12)
                          (coordY p - 12)
                          25 25
          )
          $ pointers event



foreign import javascript unsafe "$1.getContext(\"2d\")"
    get2dContext :: HTMLElement -> IO JSVal

foreign import javascript unsafe "$1.fillRect($2,$3,$4,$5)"
    fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.fillStyle = '#' + $2;"
    setStyle :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "$1.style.backgroundColor = '#' + $2;"
    setBGColor :: HTMLElement -> JSString -> IO ()

foreign import javascript safe "document.getElementById(\"testbutton\")"
    getButton :: IO HTMLElement

foreign import javascript unsafe "var body = document.getElementsByTagName(\"body\")[0]; \
    \ body.innerHTML = '<div style=\"width: 90%; margin: 25px; padding: 18px; \
                      \ background-color: coral; border-color: blue; border-style: solid;\"> \
                      \ <canvas id=\"cvn\" style=\"margin: 22px; \
                      \ padding: 10px 20px 30px 25px; border-color: darkred; border-style: dashed; \
                      \ background-color: white; width: 65%; height: 400px;\"></canvas></div> \
                      \ <button id=\"testbutton\">click me!</button>';\
    \ $r = document.getElementById(\"cvn\");"
    addCanvasToBody :: IO HTMLElement


