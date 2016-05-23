{-# LANGUAGE DeriveDataTypeable,GeneralizedNewtypeDeriving, TypeFamilies, CPP #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.TypedArray.Types
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module JsHs.TypedArray.Types where

import Data.Word
import Data.Int
import Foreign.C.Types

import Data.Typeable (Typeable)
import Data.Ix (Ix)
import Data.Data (Data)
import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)

import JsHs.LikeJS.Class
import JsHs.Internal.Types
import JsHs.Types

-- | Stub for Uint8ClampedArray in JS
newtype Word8Clamped = Clamped Word8 deriving
    (Ord,Num,Eq,Bounded,Enum,Integral,Data,Real,Show,Ix,FiniteBits,Bits,Storable)



type TypedArray a = SomeTypedArray 'Immutable a
type STTypedArray s a = SomeTypedArray ('STMutable s) a
type IOTypedArray a = SomeTypedArray 'Mutable a

-- | Any typed array, mutable or immutable
newtype SomeTypedArray (m :: MutabilityType s) (a :: *) = SomeTypedArray JSVal deriving Typeable
instance IsJSVal (SomeTypedArray m a)

--instance PToJSVal (SomeTypedArray m a) where
--  pToJSVal (SomeTypedArray v) = v
--instance PFromJSVal (SomeTypedArray m a) where
--  pFromJSVal = SomeTypedArray

-- | ArrayBuffer, mutable or immutable
newtype SomeArrayBuffer (a :: MutabilityType s) = SomeArrayBuffer JSVal deriving Typeable
instance IsJSVal (SomeArrayBuffer m)

type ArrayBuffer      = SomeArrayBuffer 'Immutable
type IOArrayBuffer    = SomeArrayBuffer 'Mutable
type STArrayBuffer s  = SomeArrayBuffer ('STMutable s)

--instance PToJSVal (SomeArrayBuffer m) where
--    pToJSVal (SomeArrayBuffer b) = b
--instance PFromJSVal (SomeArrayBuffer m) where
--    pFromJSVal = SomeArrayBuffer

-- | Data view on ArrayBuffer, mutable or immutable
newtype SomeDataView (a :: MutabilityType s) = SomeDataView JSVal deriving Typeable
instance IsJSVal (SomeDataView m)


type DataView     = SomeDataView 'Immutable
type IODataView   = SomeDataView 'Mutable
type STDataView s = SomeDataView ('STMutable s)

--instance PToJSVal (SomeDataView m) where
--    pToJSVal (SomeDataView b) = b
--instance PFromJSVal (SomeDataView m) where
--    pFromJSVal = SomeDataView

-----------------------------------------------------------------------------
-- Our types look LikeJS TypedArrays
-----------------------------------------------------------------------------


#define TYPEDARRAYLIKEJS(T,JSArrayType)\
instance LikeJS "JSArrayType" (SomeTypedArray m T) where{\
    {-# INLINE asJSVal #-};\
    asJSVal (SomeTypedArray x) = x;\
    {-# INLINE asLikeJS #-};\
    asLikeJS  = SomeTypedArray}

TYPEDARRAYLIKEJS(Int,Int32Array)
TYPEDARRAYLIKEJS(Int32,Int32Array)
TYPEDARRAYLIKEJS(Int16,Int16Array)
TYPEDARRAYLIKEJS(Int8,Int8Array)
TYPEDARRAYLIKEJS(Word,Uint32Array)
TYPEDARRAYLIKEJS(Word32,Uint32Array)
TYPEDARRAYLIKEJS(Word16,Uint16Array)
TYPEDARRAYLIKEJS(Word8,Uint8Array)
TYPEDARRAYLIKEJS(Word8Clamped,Uint8ClampedArray)
TYPEDARRAYLIKEJS(Float,Float32Array)
TYPEDARRAYLIKEJS(Double,Float64Array)
TYPEDARRAYLIKEJS(CChar,Int8Array)
TYPEDARRAYLIKEJS(CSChar,Int8Array)
TYPEDARRAYLIKEJS(CUChar,Uint8Array)
TYPEDARRAYLIKEJS(CShort,Int16Array)
TYPEDARRAYLIKEJS(CUShort,Uint16Array)
TYPEDARRAYLIKEJS(CInt,Int32Array)
TYPEDARRAYLIKEJS(CUInt,Uint32Array)
TYPEDARRAYLIKEJS(CLong,Int32Array)
TYPEDARRAYLIKEJS(CULong,Uint32Array)
TYPEDARRAYLIKEJS(CFloat,Float32Array)
TYPEDARRAYLIKEJS(CDouble,Float64Array)
