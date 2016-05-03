{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHCJS.LikeJS.Class
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}

module GHCJS.LikeJS.Class
    ( LikeJS (..)
    ) where

-- for Number types
import Data.Int
import Data.Word
import Foreign.C.Types

--
import GHC.TypeLits
import Data.Coerce (Coercible ()) --, coerce)
import Data.JSString
--import GHC.Exts (Any)
import GHCJS.Types (JSVal) --, IsJSVal)
import Unsafe.Coerce (unsafeCoerce)




-- | Describes direct representation of data types in JS and HS
--   Default implementation works for anything coercible to JSVal
class LikeJS (jstype :: Symbol) a | a -> jstype where
    asJSVal :: a -> JSVal
    asLikeJS :: JSVal -> a

    {-# INLINE asJSVal #-}
    default asJSVal :: Coercible a JSVal => a -> JSVal
    asJSVal x = x `seq` unsafeCoerce x
    {-# INLINE asLikeJS #-}
    default asLikeJS :: Coercible JSVal a => JSVal -> a
    asLikeJS =  unsafeCoerce


-- | Popular basic types
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
foreign import javascript unsafe "$r = $1" js_asJSValChar :: Char -> JSVal
{-# INLINE js_asLikeJSChar #-}
instance LikeJS "Symbol" Char where
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

LIKEJSNum(Int)
LIKEJSNum(Int32)
LIKEJSNum(Int16)
LIKEJSNum(Int8)
LIKEJSNum(Word)
LIKEJSNum(Word32)
LIKEJSNum(Word16)
LIKEJSNum(Word8)
LIKEJSNum(Float)
LIKEJSNum(Double)
LIKEJSNum(CChar)
LIKEJSNum(CSChar)
LIKEJSNum(CUChar)
LIKEJSNum(CShort)
LIKEJSNum(CUShort)
LIKEJSNum(CInt)
LIKEJSNum(CUInt)
LIKEJSNum(CLong)
LIKEJSNum(CULong)
LIKEJSNum(CFloat)
LIKEJSNum(CDouble)

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
    asJSVal Nothing  = js_null
    asJSVal (Just a) = asJSVal a
    asLikeJS val = if js_isNullOrUndef val
                   then Nothing
                   else Just $ asLikeJS val

--
--debugVal :: JSVal -> a -> a
--debugVal v a = v `seq` debugVal' v `seq` a
--
--{-# NOINLINE debugVal' #-}
--foreign import javascript unsafe "console.log($1)" debugVal' :: JSVal -> ()

-------------------------------------------------------------------------------------
-- Primititive js functions that most likely exist in GHCJS.Prim or elseware,
-- but too simple to import from outside
-------------------------------------------------------------------------------------

foreign import javascript unsafe "$r = null" js_null :: JSVal
foreign import javascript unsafe "$1 == null" js_isNullOrUndef :: JSVal -> Bool

