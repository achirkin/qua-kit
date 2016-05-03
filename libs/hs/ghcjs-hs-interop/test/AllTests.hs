{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Test.Framework

import qualified GHCJS.LikeJS.Test.TestBasicTypes


main :: IO ()
main = htfMain
  [ GHCJS.LikeJS.Test.TestBasicTypes.tests
  ]
