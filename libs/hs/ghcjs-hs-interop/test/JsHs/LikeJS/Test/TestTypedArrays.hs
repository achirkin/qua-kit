{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JsHs.LikeJS.Test.TestTypedArrays
    ( tests
    ) where



import JsHs.Types (JSVal)

import Data.Int
import Data.Word

import Test.Framework
import JsHs.LikeJS.Test.TH (writeTests)

import JsHs.LikeJS.Class
import GHC.TypeLits (KnownSymbol)

import JsHs.JSString
import JsHs.TypedArray


--import JsHs.Debug

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

type Int8Array = TypedArray Int8
type Uint8Array = TypedArray Word8
type Uint8ClampedArray = TypedArray Word8Clamped
type Int16Array = TypedArray Int16
type Uint16Array = TypedArray Word16
type Int32Array = TypedArray Int32
type Uint32Array = TypedArray Word32
type Float32Array = TypedArray Float
type Float64Array = TypedArray Double

instance Arbitrary Word8Clamped where arbitrary = Clamped <$> arbitrary


instance Arbitrary Int8Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Uint8Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Uint8ClampedArray where arbitrary = fromList <$> arbitrary
instance Arbitrary Int16Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Uint16Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Int32Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Uint32Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Float32Array where arbitrary = fromList <$> arbitrary
instance Arbitrary Float64Array where arbitrary = fromList <$> arbitrary

$(writeTests "likeJSConversion"
  [ ''Int8Array, ''Uint8Array, ''Int16Array, ''Uint16Array
  , ''Int32Array, ''Uint32Array, ''Uint8ClampedArray
  , ''Float32Array, ''Float64Array ]
    (\t ->
        [d| prop_doubleConversion :: $(t) -> Bool
            prop_doubleConversion = doubleConversion
        |]
    )
 )

$(writeTests "constructorName"
  [ ''Int8Array, ''Uint8Array, ''Int16Array, ''Uint16Array
  , ''Int32Array, ''Uint32Array, ''Uint8ClampedArray
  , ''Float32Array, ''Float64Array ]
    (\t ->
        [d| prop_JSTypeName :: $(t) -> Bool
            prop_JSTypeName = testJSTypeName
        |]
    )
 )


tests :: [TestSuite]
tests = [likeJSConversion, constructorName]
