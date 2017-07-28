{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Home.LoadingSplash
  ( loadingSplash
  ) where

import Import

-- | Rotating circular progress bar;
--   exposes one dom id '#loadingSplash'.
loadingSplash :: Widget
loadingSplash = do
  redCircle <- newIdent
  greyCircle <- newIdent
  rotRed <- newIdent
  rotGrey <- newIdent
  toWidgetHead
    [cassius|
      #loadingSplash
        z-index: 777
        display: block
        position: fixed
        height: 40%
        padding: 0
        top: 50%
        left: 50%
        margin: 0 -50% 0 0
        transform: translate(-50%, -50%)

      ##{redCircle}
        position: relative
        transform-origin: 36px 36px
        -ms-transform-origin: 36px 36px
        -moz-transform-origin: 36px 36px
        -webkit-transform-origin: 36px 36px
        -webkit-animation: #{rotRed} 32s linear infinite
        -moz-animation: #{rotRed} 3s linear infinite
        -ms-animation: #{rotRed} 3s linear infinite
        -o-animation: #{rotRed} 3s linear infinite
        animation: #{rotRed} 3s linear infinite

      ##{greyCircle}
        position: relative
        transform-origin: 36px 36px
        -ms-transform-origin: 36px 36px
        -moz-transform-origin: 36px 36px
        -webkit-transform-origin: 36px 36px
        -webkit-animation: #{rotGrey} 8s linear infinite
        -moz-animation: #{rotGrey} 8s linear infinite
        -ms-animation: #{rotGrey} 8s linear infinite
        -o-animation: #{rotGrey} 8s linear infinite
        animation: #{rotGrey} 8s linear infinite

      @-webkit-keyframes #{rotRed}
        from
          -ms-transform: rotate(0deg)
          -moz-transform: rotate(0deg)
          -webkit-transform: rotate(0deg)
          -o-transform: rotate(0deg)
          transform: rotate(0deg)
        to
          -ms-transform: rotate(360deg)
          -moz-transform: rotate(360deg)
          -webkit-transform: rotate(360deg)
          -o-transform: rotate(360deg)
          transform: rotate(360deg)

      @keyframes #{rotRed}
        from
          -ms-transform: rotate(0deg)
          -moz-transform: rotate(0deg)
          -webkit-transform: rotate(0deg)
          -o-transform: rotate(0deg)
          transform: rotate(0deg)
        to
          -ms-transform: rotate(360deg)
          -moz-transform: rotate(360deg)
          -webkit-transform: rotate(360deg)
          -o-transform: rotate(360deg)
          transform: rotate(360deg)

      @-webkit-keyframes #{rotGrey}
        from
          -ms-transform: rotate(360deg)
          -moz-transform: rotate(360deg)
          -webkit-transform: rotate(360deg)
          -o-transform: rotate(360deg)
          transform: rotate(360deg)
        to
          -ms-transform: rotate(0deg)
          -moz-transform: rotate(0deg)
          -webkit-transform: rotate(0deg)
          -o-transform: rotate(0deg)
          transform: rotate(0deg)

      @keyframes #{rotGrey}
        from
          -ms-transform: rotate(360deg)
          -moz-transform: rotate(360deg)
          -webkit-transform: rotate(360deg)
          -o-transform: rotate(360deg)
          transform: rotate(360deg)
        to
          -ms-transform: rotate(0deg)
          -moz-transform: rotate(0deg)
          -webkit-transform: rotate(0deg)
          -o-transform: rotate(0deg)
          transform: rotate(0deg)
    |]
  toWidgetBody
    [hamlet|
      <svg fill="none" #loadingSplash version="1.1" viewBox="0 0 72 72" xmlns="http://www.w3.org/2000/svg">
        <circle cx="36" cy="36" ##{redCircle} r="28" stroke="#FF5722" stroke-dasharray="10, 5, 50, 40, 30.929188601, 40" stroke-opacity="1" stroke-width="16">
        <circle cx="36" cy="36" ##{greyCircle} r="28" stroke="#BF360C" stroke-dasharray="38, 14, 8, 14, 65.929188601, 14, 8, 14" stroke-opacity=".2" stroke-width="8">
    |]
