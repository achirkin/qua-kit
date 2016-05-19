{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.TypedArray.ST
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- Mutable operatons on JsHs.typed arrays in ST monad
--
-----------------------------------------------------------------------------

module JsHs.TypedArray.ST where


-- import qualified GHC.Types as Exts
import GHC.ST

import Control.Monad.Primitive (PrimState (..))

import Data.Word
import Data.Int
import Data.Primitive.ByteArray (MutableByteArray (..)) -- ByteArray (..),
import Foreign.C.Types
import Unsafe.Coerce (unsafeCoerce)

import JsHs.Internal.Types

import JsHs.TypedArray
--import JsHs.TypedArray.Types
import JsHs.TypedArray.Internal

-----------------------------------------------------------------------------
-- | mutable typed arrays
-----------------------------------------------------------------------------

class STTypedArrayOperations a where
    -- | Init a new typed array filled with zeroes
    newSTTypedArray :: Int -> ST s (STTypedArray s a)
    -- | Fill a new typed array with a given value
    fillNewSTTypedArray :: Int -> a -> ST s (STTypedArray s a)
    -- | Create a new typed array from list
    newFromList :: [a] -> ST s (STTypedArray s a)
    -- | Create a new typed array from elements of another typed array
    newFromArray :: SomeTypedArray (m :: MutabilityType sk) b -> ST s (STTypedArray s a)
    -- | Get value from array at specified index
    index :: Int -> STTypedArray s a -> ST s a
    -- | Set value into array at specified index
    setIndex ::Int -> a -> STTypedArray s a -> ST s ()
    -- | Set list into array with specified offset
    setList :: Int -> [a] -> STTypedArray s a -> ST s ()
    -- | Set array elements into array with specified offset
    setArray :: Int -> SomeTypedArray (m :: MutabilityType sk) b -> STTypedArray s a -> ST s ()


#define TYPEDARRAY(T,JSType,JSSize)\
instance STTypedArrayOperations T where{\
    {-# INLINE newSTTypedArray #-};\
    newSTTypedArray n = ST (js_createM/**/T/**/Array n);\
    {-# INLINE fillNewSTTypedArray #-};\
    fillNewSTTypedArray n v = ST (js_fillNewM/**/T/**/Array n v);\
    {-# INLINE newFromList #-};\
    newFromList xs = ST (js_fromListM/**/T/**/Array . unsafeCoerce . seqList $ xs);\
    {-# INLINE newFromArray #-};\
    newFromArray arr = ST (js_fromArrayM/**/T/**/Array arr);\
    {-# INLINE index #-};\
    index i arr = ST (js_getIndex/**/T/**/Array i arr);\
    {-# INLINE setIndex #-};\
    setIndex i v arr = ST (js_setIndex/**/T/**/Array i v arr);\
    {-# INLINE setList #-};\
    setList offset xs arr = ST (js_setList/**/T/**/Array offset (unsafeCoerce $ seqList xs) arr);\
    {-# INLINE setArray #-};\
    setArray offset ar0 arr = ST (js_setArray/**/T/**/Array offset ar0 arr)}


TYPEDARRAY(Int,Int32,4)
TYPEDARRAY(Int32,Int32,4)
TYPEDARRAY(Int16,Int16,2)
TYPEDARRAY(Int8,Int8,1)
TYPEDARRAY(Word,Uint32,4)
TYPEDARRAY(Word32,Uint32,4)
TYPEDARRAY(Word16,Uint16,2)
TYPEDARRAY(Word8,Uint8,1)
TYPEDARRAY(Word8Clamped,Uint8Clamped,1)
TYPEDARRAY(Float,Float32,4)
TYPEDARRAY(Double,Float64,8)
TYPEDARRAY(CChar,Int8,1)
TYPEDARRAY(CSChar,Int8,1)
TYPEDARRAY(CUChar,Uint8,1)
TYPEDARRAY(CShort,Int16,2)
TYPEDARRAY(CUShort,Uint16,2)
TYPEDARRAY(CInt,Int32,4)
TYPEDARRAY(CUInt,Uint32,4)
TYPEDARRAY(CLong,Int32,4)
TYPEDARRAY(CULong,Uint32,4)
TYPEDARRAY(CFloat,Float32,4)
TYPEDARRAY(CDouble,Float64,8)


-----------------------------------------------------------------------------
-- | mutable anything
-----------------------------------------------------------------------------




class STArrayBufferData s mutable any | any s -> mutable where
    -- | Slice array (elements) or buffer (bytes).
    --   See documentation on TypedArray.prototype.slice() and ArrayBuffer.prototype.slice()
    slice :: Int -> Maybe Int -> any -> ST s mutable


class ( MutableArrayBufferPrim mutable
      ) => STArrayBufferConversions s immutable mutable
        | s immutable -> mutable
        , mutable -> immutable s where
    -- | Create an immutable data by copying a mutable data
    freeze :: mutable -> ST s immutable
    -- | Create an immutable data from a mutable data without
    --   copying. The result shares the buffer with the argument, do not modify
    --   the data in the original buffer after freezing
    unsafeFreeze :: mutable -> ST s immutable
    -- | Create a mutable data by copying an immutable data
    thaw :: immutable -> ST s mutable
    -- | Create a mutable data from an immutable data without
    --   copying. The result shares the buffer with the argument.
    unsafeThaw :: immutable -> ST s mutable
    -- | Convert from MutableByteArray without copying data
    fromMutableByteArray :: MutableByteArray (PrimState (ST s)) -> ST s mutable
    fromMutableByteArray (MutableByteArray ba) = ST (fromMutableByteArrayPrim ba)
    {-# INLINE fromMutableByteArray #-}
    -- | Convert to MutableByteArray without copying data
    toMutableByteArray :: mutable -> ST s (MutableByteArray (PrimState (ST s)))
    toMutableByteArray b = ST $ \s ->
        case toMutableByteArrayPrim b s of (# s1, ba #) -> (# s1, MutableByteArray ba #)
    {-# INLINE toMutableByteArray #-}


instance STArrayBufferData s (STArrayBuffer s) (SomeArrayBuffer m) where
    {-# INLINE slice #-}
    slice i0 Nothing (SomeArrayBuffer b) = fmap SomeArrayBuffer . ST $ js_slice1 i0 b
    slice i0 (Just i1) (SomeArrayBuffer b) = fmap SomeArrayBuffer . ST $ js_slice i0 i1 b

instance STArrayBufferConversions s ArrayBuffer (STArrayBuffer s) where
    {-# INLINE freeze #-}
    freeze (SomeArrayBuffer b) = fmap SomeArrayBuffer (ST (js_slice1 0 b))
    {-# INLINE unsafeFreeze #-}
    unsafeFreeze (SomeArrayBuffer b) = pure (SomeArrayBuffer b)
    {-# INLINE thaw #-}
    thaw (SomeArrayBuffer b) = fmap SomeArrayBuffer (ST (js_slice1 0 b))
    {-# INLINE unsafeThaw #-}
    unsafeThaw (SomeArrayBuffer b) = pure (SomeArrayBuffer b)


instance STArrayBufferData s (STTypedArray s t) (SomeTypedArray m t) where
    {-# INLINE slice #-}
    slice i0 Nothing (SomeTypedArray b) = fmap SomeTypedArray . ST $ js_slice1 i0 b
    slice i0 (Just i1) (SomeTypedArray b) = fmap SomeTypedArray . ST $ js_slice i0 i1 b

instance ( MutableArrayBufferPrim (STTypedArray s t)
         ) => STArrayBufferConversions s (TypedArray t) (STTypedArray s t) where
    {-# INLINE freeze #-}
    freeze (SomeTypedArray b) = fmap SomeTypedArray (ST (js_slice1 0 b))
    {-# INLINE unsafeFreeze #-}
    unsafeFreeze (SomeTypedArray b) = pure (SomeTypedArray b)
    {-# INLINE thaw #-}
    thaw (SomeTypedArray b) = fmap SomeTypedArray (ST (js_slice1 0 b))
    {-# INLINE unsafeThaw #-}
    unsafeThaw (SomeTypedArray b) = pure (SomeTypedArray b)


instance STArrayBufferConversions s DataView (STDataView s) where
    {-# INLINE freeze #-}
    freeze dv = ST (js_cloneDataView dv)
    {-# INLINE unsafeFreeze #-}
    unsafeFreeze (SomeDataView b) = pure (SomeDataView b)
    {-# INLINE thaw #-}
    thaw dv = ST (js_cloneDataView dv)
    {-# INLINE unsafeThaw #-}
    unsafeThaw (SomeDataView b) = pure (SomeDataView b)

#define DATAVIEW8(T,JSType,JSSize)\
write/**/T, unsafeWrite/**/T\
  :: Int -> T -> STDataView s -> ST s ();\
write/**/T       idx x dv = ST (js_safeSet/**/T   idx x dv);\
unsafeWrite/**/T idx x dv = ST (js_unsafeSet/**/T idx x dv);\
{-# INLINE write/**/T #-};\
{-# INLINE unsafeWrite/**/T #-};\
read/**/T, unsafeRead/**/T\
  :: Int -> STDataView s -> ST s T;\
read/**/T       idx dv = ST (js_m_safeGet/**/T   idx dv);\
unsafeRead/**/T idx dv = ST (js_m_unsafeGet/**/T idx dv);\
{-# INLINE read/**/T #-};\
{-# INLINE unsafeRead/**/T #-};

#define DATAVIEW(T,JSType,JSSize)\
write/**/T/**/LE, write/**/T/**/BE, unsafeWrite/**/T/**/LE, unsafeWrite/**/T/**/BE, write/**/T, unsafeWrite/**/T\
  :: Int -> T -> STDataView s -> ST s ();\
write/**/T/**/LE       idx x dv = ST (js_safeSet/**/T/**/LE   idx x dv);\
write/**/T/**/BE       idx x dv = ST (js_safeSet/**/T/**/BE   idx x dv);\
unsafeWrite/**/T/**/LE idx x dv = ST (js_unsafeSet/**/T/**/LE idx x dv);\
unsafeWrite/**/T/**/BE idx x dv = ST (js_unsafeSet/**/T/**/BE idx x dv);\
{- | Shortcut for little-endian -};\
write/**/T       = write/**/T/**/LE;\
{- | Shortcut for little-endian -};\
unsafeWrite/**/T = unsafeWrite/**/T/**/LE;\
{-# INLINE write/**/T/**/LE #-};\
{-# INLINE write/**/T/**/BE #-};\
{-# INLINE unsafeWrite/**/T/**/LE #-};\
{-# INLINE unsafeWrite/**/T/**/BE #-};\
{-# INLINE write/**/T #-};\
{-# INLINE unsafeWrite/**/T #-};\
read/**/T/**/LE, read/**/T/**/BE, unsafeRead/**/T/**/LE, unsafeRead/**/T/**/BE, read/**/T, unsafeRead/**/T\
  :: Int -> STDataView s -> ST s T;\
read/**/T/**/LE       idx dv = ST (js_m_safeGet/**/T/**/LE   idx dv);\
read/**/T/**/BE       idx dv = ST (js_m_safeGet/**/T/**/BE   idx dv);\
unsafeRead/**/T/**/LE idx dv = ST (js_m_unsafeGet/**/T/**/LE idx dv);\
unsafeRead/**/T/**/BE idx dv = ST (js_m_unsafeGet/**/T/**/BE idx dv);\
{- | Shortcut for little-endian -};\
read/**/T       = read/**/T/**/LE;\
{- | Shortcut for little-endian -};\
unsafeRead/**/T = unsafeRead/**/T/**/LE;\
{-# INLINE read/**/T/**/LE #-};\
{-# INLINE read/**/T/**/BE #-};\
{-# INLINE unsafeRead/**/T/**/LE #-};\
{-# INLINE unsafeRead/**/T/**/BE #-};\
{-# INLINE read/**/T #-};\
{-# INLINE unsafeRead/**/T #-};

DATAVIEW(Int,Int32,4)
DATAVIEW(Int32,Int32,4)
DATAVIEW(Int16,Int16,2)
DATAVIEW(Word,Uint32,4)
DATAVIEW(Word32,Uint32,4)
DATAVIEW(Word16,Uint16,2)
DATAVIEW(Float,Float32,4)
DATAVIEW(Double,Float64,8)

DATAVIEW8(Word8,Uint8,1)
DATAVIEW8(Int8,Int8,1)

-----------------------------------------------------------------------------
-- Misc
-----------------------------------------------------------------------------

-- | Create new array buffer
newSTArrayBuffer :: Int -> ST s (STArrayBuffer s)
newSTArrayBuffer size = ST (js_createArrayBuffer size)

