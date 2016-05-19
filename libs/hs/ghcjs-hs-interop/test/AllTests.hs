module Main (main) where

import Test.Framework

import qualified JsHs.LikeJS.Test.TestBasicTypes
import qualified JsHs.LikeJS.Test.TestTypedArrays

--import JsHs.Debug
--import JsHs.LikeJS.Class
--
--import JsHs.TypedArray

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
--
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

--  let a = fromList [0, 345.23, 22, -0.23412262, 333] :: TypedArray Double
--      j = asJSVal a
--      b = asLikeJS j :: TypedArray Double
--  print a
--  printAny a
--  printJSVal j
--  printAny b

  htfMain
    [ JsHs.LikeJS.Test.TestBasicTypes.tests
    , JsHs.LikeJS.Test.TestTypedArrays.tests
    ]
