{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JsHs.LikeJS.Test.TestBasicTypes
    ( tests
    ) where



import JsHs.Types (JSVal)

import Data.Int
import Data.Word
import JsHs.JSString

import Test.Framework
import JsHs.LikeJS.Test.TH (writeTests)

import JsHs.LikeJS.Class
import GHC.TypeLits (KnownSymbol)

--import JsHs.LikeJS.Test.Debug

instance Arbitrary JSString where
    arbitrary = pack <$> arbitrary

--import JsHs.LikeJS.Test.Debug
--
--doubleConversion :: (LikeJS ta a, Eq a, Show a) => a -> Bool
--doubleConversion x = traceShow "Testing values" . traceShow x . debugAny x . debugJSVal y . debugAny z . traceShow z $ z == x
--  where
--    y = asJSVal x
--    z = asLikeJS y

doubleConversion :: (LikeJS ta a, Eq a) => a -> Bool
doubleConversion x = (asLikeJS $ asJSVal x) == x

testJSTypeName :: (KnownSymbol ta, LikeJS ta a) => a -> Bool
testJSTypeName x = jsTypeName x == unpack (js_getConstructorName $ asJSVal x)

{-# NOINLINE js_getConstructorName #-}
foreign import javascript unsafe "($1).constructor.name" js_getConstructorName :: JSVal -> JSString

type DoubleList = [Double]
type JSStringList = [JSString]

$(writeTests "likeJSConversion"
  [ ''Int, ''Int32, ''Int16, ''Int8, ''Word, ''Word32, ''Word16, ''Word8
  , ''Float, ''Double, ''Bool, ''Char, ''Integer, ''JSString
  , ''String, ''DoubleList, ''JSStringList ]
    (\t ->
        [d| prop_doubleConversion :: $(t) -> Bool
            prop_doubleConversion = doubleConversion
        |]
    )
 )

$(writeTests "constructorName"
  [ ''Int, ''Int32, ''Int16, ''Int8, ''Word, ''Word32, ''Word16, ''Word8
  , ''Float, ''Double, ''Bool, ''Char, ''Integer, ''JSString
  , ''String, ''DoubleList]
    (\t ->
        [d| prop_JSTypeName :: $(t) -> Bool
            prop_JSTypeName = testJSTypeName
        |]
    )
 )


tests :: [TestSuite]
tests = [likeJSConversion, constructorName]
