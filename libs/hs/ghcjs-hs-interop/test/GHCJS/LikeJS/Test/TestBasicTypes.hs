{-# LANGUAGE TemplateHaskell #-}
module GHCJS.LikeJS.Test.TestBasicTypes
    ( tests
    ) where



--import GHCJS.Types (JSVal)
--import Debug.Trace (traceShow)

import Data.Int
import Data.Word
--import Data.JSString


import Test.Framework
import GHCJS.LikeJS.Test.TH (writeTests)

import GHCJS.LikeJS.Class


--instance Arbitrary JSString where
--    arbitrary = do
--

--debugVal :: JSVal -> a -> a
--debugVal v a = v `seq` debugVal' v `seq` a
--
--{-# NOINLINE debugVal' #-}
--foreign import javascript unsafe "console.log($1)" debugVal' :: JSVal -> ()

--doubleConversion :: (LikeJS ta a, Eq a, Show a) => a -> Bool
--doubleConversion x = traceShow "Testing values" . traceShow x . debugVal y . traceShow z $ z == x
--  where
--    y = asJSVal x
--    z = asLikeJS y

doubleConversion :: (LikeJS ta a, Eq a) => a -> Bool
doubleConversion x = (asLikeJS $ asJSVal x) == x

$(writeTests "likeJSConversion"
  [ ''Int, ''Int32, ''Int16, ''Int8, ''Word, ''Word32, ''Word16, ''Word8
  , ''Float, ''Double, ''Bool, ''Char]
    (\t ->
        [d| prop_doubleConversion :: $(t) -> Bool
            prop_doubleConversion = doubleConversion
        |]
    )
 )


tests :: [TestSuite]
tests = [likeJSConversion]
