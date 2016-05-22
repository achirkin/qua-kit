{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash, UnboxedTuples, UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.LikeJS.Class
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module JsHs.LikeJS.Class
    ( LikeJS (..)
    , jsTypeName
    ) where

-- for Number types
import Data.Int
import Data.Word

--
import GHC.TypeLits
import GHC.Prim
--import Data.Coerce (Coercible ()) --, coerce)
import Data.JSString
--import GHC.Exts (Any)
import JsHs.Types.Prim (JSVal, jsNull, jsIsNullOrUndef)
import Unsafe.Coerce (unsafeCoerce)
import Control.DeepSeq (deepseq)


--import Data.Foldable (foldr')


-- | Describes direct representation of data types in JS and HS.
--   Default implementation works for anything coercible to JSVal.
--   Name in LikeJS 'Name' must correspond to JS ($1).constructor.name
class LikeJS (jstype :: Symbol) a | a -> jstype where
    asJSVal :: a -> JSVal
    asLikeJS :: JSVal -> a

    {-# INLINE asJSVal #-}
    default asJSVal :: Coercible a JSVal => a -> JSVal
    asJSVal x = x `seq` coerce x
    {-# INLINE asLikeJS #-}
    default asLikeJS :: Coercible JSVal a => JSVal -> a
    asLikeJS =  coerce

-- | Show the name of a corresponding JsHs.type
{-# INLINE jsTypeName #-}
jsTypeName :: (KnownSymbol jstype, LikeJS jstype a) => a -> String
jsTypeName x = symbolVal' (jsTypeProxy x)

{-# INLINE jsTypeProxy #-}
jsTypeProxy :: (KnownSymbol jstype, LikeJS jstype a) => a -> Proxy# jstype
jsTypeProxy _ = proxy#



-------------------------------------------------------------------------------------
-- Basic Haskell value types
-------------------------------------------------------------------------------------

-- Damn String overlaps with [Char]
--instance LikeJS "String" String where
--    {-# INLINE asJSVal #-}
--    asJSVal  = asJSVal . pack
--    {-# INLINE asLikeJS #-}
--    asLikeJS =  unpack . asLikeJS

instance LikeJS "String" JSString where
    {-# INLINE asJSVal #-}
    asJSVal x = x `seq` unsafeCoerce x
    {-# INLINE asLikeJS #-}
    asLikeJS  =  unsafeCoerce

foreign import javascript unsafe "$r = $1" js_asLikeJSBool :: JSVal -> Bool
{-# INLINE js_asLikeJSBool #-}
foreign import javascript unsafe "$r = $1" js_asJSValBool :: Bool -> JSVal
{-# INLINE js_asJSValBool #-}
instance LikeJS "Boolean" Bool where
    asJSVal x = x `seq` js_asJSValBool x
    asLikeJS  =  js_asLikeJSBool

foreign import javascript unsafe "$r = $1" js_asLikeJSChar :: JSVal -> Char
{-# INLINE js_asLikeJSChar #-}
foreign import javascript unsafe "$r = $1" js_asJSValChar :: Char -> JSVal
{-# INLINE js_asJSValChar #-}
instance LikeJS "Number" Char where
    asJSVal x = x `seq` js_asJSValChar x
    asLikeJS  =  js_asLikeJSChar



-- | convert Number types
#define LIKEJSNum(T) \
    foreign import javascript unsafe "$r = $1" \
        asJSVal/**/T :: T -> JSVal;  {-# INLINE asJSVal/**/T #-}; \
    foreign import javascript unsafe "$r = $1" \
        js_asLikeJS/**/T :: JSVal -> T;  {-# INLINE js_asLikeJS/**/T #-}; \
    instance LikeJS "Number" T where { \
        asJSVal x = x `seq` asJSVal/**/T x; {-# INLINE asJSVal #-}; \
        asLikeJS  = js_asLikeJS/**/T; {-# INLINE asLikeJS #-}; }


{-# WARNING js_asLikeJSInt "JsHs.will produce an incorrect result on numbers longer than 32 bit" #-}
{-# WARNING asJSValInt     "JsHs.will produce an incorrect result on numbers longer than 32 bit" #-}
LIKEJSNum(Int)
-- LIKEJSNum(Int64)  -- stored differently
LIKEJSNum(Int32)
LIKEJSNum(Int16)
LIKEJSNum(Int8)
LIKEJSNum(Word)
-- LIKEJSNum(Word64) -- stored differently
LIKEJSNum(Word32)
LIKEJSNum(Word16)
LIKEJSNum(Word8)
LIKEJSNum(Float)
LIKEJSNum(Double)


-- have to convert to BigInteger if the value is less than 32 bit width
-- to preserve JS type name
foreign import javascript unsafe "$1.d2 == null ? h$bigFromInt($1.d1) : $1.d2"
    js_asJSValInteger :: Any -> JSVal
{-# INLINE js_asJSValInteger #-}
foreign import javascript unsafe "$r = h$integer_mpzToInteger($1)"
    js_asLikeJSInteger :: JSVal -> Any
{-# INLINE js_asLikeJSInteger #-}
instance LikeJS "BigInteger" Integer where
    asJSVal x = x `deepseq` js_asJSValInteger (unsafeCoerce x)
    asLikeJS = unsafeCoerce . js_asLikeJSInteger


-------------------------------------------------------------------------------------
-- Basic Haskell container types
-------------------------------------------------------------------------------------


instance (LikeJS ta a, LikeJS tb b) => LikeJS "LikeHS.Either" (Either a b) where
    asJSVal (Left a)  = js_Either (asJSVal a) True
    asJSVal (Right b) = js_Either (asJSVal b) False
    asLikeJS val = if js_Either_isRight val
                   then Right . asLikeJS $ js_Either_right val
                   else Left . asLikeJS $ js_Either_left val

foreign import javascript unsafe "new LikeHS.Either($1, $2)" js_Either :: JSVal -> Bool -> JSVal
foreign import javascript unsafe "$1.isRight()" js_Either_isRight :: JSVal -> Bool
foreign import javascript unsafe "$1.right" js_Either_right :: JSVal -> JSVal
foreign import javascript unsafe "$1.left"  js_Either_left  :: JSVal -> JSVal


instance (LikeJS ta a) => LikeJS ta (Maybe a) where
    asJSVal Nothing  = jsNull
    asJSVal (Just a) = asJSVal a
    asLikeJS val = if jsIsNullOrUndef val
                   then Nothing
                   else Just $ asLikeJS val

instance (LikeJS ta a) => LikeJS "Array" [a] where
    asJSVal xs = f xs js_empty_array
      where f [] a = a
            f (e:es) a = case js_push a (asJSVal e) of
                             a1 -> f es a1
    asLikeJS val = if jsIsNullOrUndef val || n <= 0
                   then []
                   else f val [] (n-1)
      where f js xs i = case asLikeJS (js_getArrIdx js i) : xs of
                          ys -> if i <= 0
                                then ys
                                else f js ys (i-1)
            n = js_getArrLength val

foreign import javascript unsafe "$r = new Array();"
    js_empty_array :: JSVal
foreign import javascript unsafe "$r = $1; $r.push($2);"
    js_push :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "$r = $1[$2];"
    js_getArrIdx :: JSVal  -> Int -> JSVal
foreign import javascript unsafe "$r = $1.length;"
    js_getArrLength :: JSVal -> Int

-- convert Strings and JSStrings
--
--{-# RULES
--"asJSVal/String"   asJSVal = js_toJSString . unsafeCoerce . seqList :: String -> JSVal
--"asLikeJS/String" asLikeJS = unsafeCoerce . js_fromJSString :: JSVal -> String
--    #-}
--
--{-# INLINE js_fromJSString #-}
--foreign import javascript unsafe "h$toHsString($1)"
--  js_fromJSString :: JSVal -> Any
--
--{-# INLINE js_toJSString #-}
--foreign import javascript unsafe "h$fromHsString($1)"
--  js_toJSString :: Any -> JSVal
--
---- reduce the spine and all list elements to whnf
--seqList :: [a] -> [a]
--seqList xs = foldr' seq () xs `seq` xs
