{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module JsHs.LikeJS.Test.TestCommonContainers
    ( tests
    ) where



-- import JsHs.Types (JSVal)
import JsHs.Types.Prim

--import Data.Int
--import Data.Word
import JsHs.JSString

import Test.Framework
import JsHs.LikeJS.Test.TH (writeTests)

import JsHs.LikeJS.Class
import GHC.TypeLits (KnownSymbol)

--import JsHs.Debug

instance Arbitrary JSString where
    arbitrary = pack <$> arbitrary

--doubleConversion :: (LikeJS ta a, Eq a, Show a) => a -> Bool
--doubleConversion x = traceShow "Testing values" . traceShow x . debugAny x . debugJSVal y . debugAny z . traceShow z $ z == x
--  where
--    y = asJSVal x
--    z = asLikeJS y

type EitherJSStringDouble = Either JSString Double
type MaybeDouble = Maybe Double
type MMMString = Maybe (Either String (Maybe String))
type EitherMBoolDouble = Either (Maybe Bool) Double

doubleConversion :: (LikeJS ta a, Eq a) => a -> Bool
doubleConversion x = (asLikeJS $ asJSVal x) == x

testJSTypeName :: (KnownSymbol ta, LikeJS ta a) => a -> Bool
testJSTypeName x = jsIsNullOrUndef y || jsTypeName x == unpack (js_getConstructorName y)
    where y = asJSVal x

{-# NOINLINE js_getConstructorName #-}
foreign import javascript unsafe "($1).constructor.name" js_getConstructorName :: JSVal -> JSString


$(writeTests "likeJSConversion"
  [ ''EitherJSStringDouble, ''MaybeDouble, ''MMMString, ''EitherMBoolDouble ]
    (\t ->
        [d| prop_doubleConversion :: $(t) -> Bool
            prop_doubleConversion = doubleConversion
        |]
    )
 )

$(writeTests "constructorName"
  [ ''EitherJSStringDouble, ''MaybeDouble, ''MMMString, ''EitherMBoolDouble ]
    (\t ->
        [d| prop_JSTypeName :: $(t) -> Bool
            prop_JSTypeName = testJSTypeName
        |]
    )
 )

tests :: [TestSuite]
tests = [likeJSConversion, constructorName]
