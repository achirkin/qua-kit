module Main (main) where

import Test.Framework

import qualified GHCJS.LikeJS.Test.TestBasicTypes

--import GHCJS.LikeJS.Test.Debug
--import GHCJS.LikeJS.Class


main :: IO ()
main = do
--  let x = 1725745734573465784 :: Integer
--  print x
--  print (1725745734573465784 :: Int)
--  printAny (1725745734573465784 :: Int)
--  let xjsval = asJSVal x
--      xback = asLikeJS xjsval :: Integer
--  putStrLn "Final stage"
--  printJSVal xjsval
--  printAny xback
--  print xback

--  let s = [11..15] :: [Int]
--      sjs = asJSVal s
--      sback = asLikeJS sjs :: [Int]
--  putStrLn "\nputStrLn"
--  print s
--  print sback
--  putStrLn "\nprintJSVal"
--  printJSVal sjs
--  putStrLn "\nprintAny"
--  printAny s
--  printAny sjs
--  printAny sback

  htfMain
    GHCJS.LikeJS.Test.TestBasicTypes.tests

