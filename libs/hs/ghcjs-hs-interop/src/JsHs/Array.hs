{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
--
-- Module      :  JsHs.Array
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- JavaScript Arrays containing elements of a single type.
-- Also provides an interface to derive behavior of arrays (if you do newtype JSVal and assume it contains an array).
-- For array-like structures instanciate `LikeJSArray` class.
-- For elements of arrays instanciate `LikeJS` class.
--
-----------------------------------------------------------------------------

module JsHs.Array
    ( Array (), emptyArray
    , LikeJSArray (..), LikeJS (..)
    , join
    , map, mapi, mapSame, mapIO, mapIO_, mapiIO, mapiIO_
    , foldl, foldl1, foldr, foldr1, foldi, foldi1
    , foldIO, foldIO_, foldiIO, foldiIO_
    , zip, zipi, zipIO, zipIO_, zipiIO, zipiIO_
    , unionZip, unionZipIO, unionZipIO_
    , fromList, toList
    , length, (!), slice, take, drop, concat
    , filter, mapEither
    , sort, removeSeqDups
    ) where

--import Data.Geometry.VectorMath --(Vector )
import Prelude hiding (map, foldl, foldr, foldl1, foldr1
                      , zip, take, drop, concat, filter, length)


import Data.Coerce (Coercible (), coerce)
import Control.Monad (void)
import System.IO.Unsafe (unsafePerformIO)

import JsHs.JSString (JSString, pack, unpack')
import JsHs.Callback
import JsHs.Types (JSVal, IsJSVal, jsval)
import JsHs.LikeJS.Class


-- | JavaScript array containing elements of single type
newtype Array a = Array JSVal
instance IsJSVal (Array a)
instance LikeJS t a => LikeJS "Array" (Array a)
instance LikeJS t a => LikeJSArray t (Array a) where
    type ArrayElem (Array a) = a
    {-# INLINE toJSArray #-}
    toJSArray = id
    {-# INLINE fromJSArray #-}
    fromJSArray = id

-- | Data types, representation of which is the same as of JavaScript Array
class LikeJS ta (ArrayElem a) => LikeJSArray ta a where
    type ArrayElem a
    toJSArray :: a -> Array (ArrayElem a)
    fromJSArray :: Array (ArrayElem a) -> a

    {-# INLINE toJSArray #-}
    default toJSArray :: Coercible a JSVal => a -> Array (ArrayElem a)
    toJSArray = coerce
    {-# INLINE fromJSArray #-}
    default fromJSArray :: Coercible JSVal a => Array (ArrayElem a) -> a
    fromJSArray = coerce


-- | Convert haskell list into JS array
fromList :: ( LikeJSArray ta a )
         => [ArrayElem a] -> a
fromList = fromJSArray . Array . asJSVal

-- | Convert JS array to haskell list
toList :: ( LikeJSArray ta a )
       => a -> [ArrayElem a]
toList = asLikeJS . jsval . toJSArray


{-# NOINLINE map #-}
map :: ( LikeJSArray ta a
       , LikeJS ty y)
    => (ArrayElem a -> y)
    -> a -> Array y
map f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
        r <- js_mapArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE mapSame #-}
mapSame :: ( LikeJSArray ta a
           , LikeJS ta x
           , x ~ ArrayElem a)
        => (x -> x)
        -> a -> a
mapSame f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
        r <- fromJSArray <$> js_mapArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE mapi #-}
mapi :: ( LikeJSArray ta a
         , LikeJS ty y)
      => (Int -> ArrayElem a -> y)
      -> a -> Array y
mapi f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \e i -> asJSVal (f (asLikeJS i) (asLikeJS e))
        r <- js_mapArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE mapIO #-}
mapIO :: ( LikeJSArray ta a
           , LikeJS ty y)
        => (ArrayElem a -> IO y)
        -> a -> IO (Array y)
mapIO f arr = do
        call <- syncCallbackUnsafeIO1 $ fmap asJSVal . f . asLikeJS
        r <- js_mapArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE mapIO_ #-}
mapIO_ :: LikeJSArray ta a => (ArrayElem a -> IO ()) -> a -> IO ()
mapIO_ f arr = do
        call <- syncCallback1 ContinueAsync $ f . asLikeJS
        js_mapArray_ call (toJSArray arr)
        releaseCallback call

{-# INLINE mapiIO #-}
mapiIO :: ( LikeJSArray ta a, LikeJS ty y) => (Int -> ArrayElem a -> IO y) -> a -> IO (Array y)
mapiIO f arr = do
        call <- syncCallbackUnsafeIO2 $ \e i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e))
        r <- js_mapArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE mapiIO_ #-}
mapiIO_ :: LikeJSArray ta a => (Int -> ArrayElem a -> IO ()) -> a -> IO ()
mapiIO_ f arr = do
        call <- syncCallback2 ContinueAsync $ \e i -> f (asLikeJS i) (asLikeJS e)
        js_mapArray_ call (toJSArray arr)
        releaseCallback call


{-# RULES
"map/concat"     forall f g arr . map     f (map g arr)     = map     (f . g) arr
"mapSame/concat" forall f g arr . mapSame f (mapSame g arr) = mapSame (f . g) arr
"mapSN/concat"   forall f g arr . map     f (mapSame g arr) = map     (f . g) arr
"map/toSame"     forall f   arr . fromJSArray (map f arr)   = mapSame f arr
    #-}


{-# NOINLINE foldl #-}
foldl :: ( LikeJSArray tt t
           , LikeJS ta a)
        => (a -> ArrayElem t -> a)
        -> a -> t -> a
foldl f x0 arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldlArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE foldl1 #-}
foldl1 :: ( LikeJSArray tt t
            , LikeJS ta a)
         => (a -> ArrayElem t -> a)
         -> t -> a
foldl1 f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldl1Array call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE foldr #-}
foldr :: ( LikeJSArray tt t
           , LikeJS ta a)
        => (ArrayElem t -> a -> a)
        -> a -> t -> a
foldr f x0 arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldrArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE foldr1 #-}
foldr1 :: ( LikeJSArray tt t
            , LikeJS ta a)
         => (ArrayElem t -> a -> a)
         -> t -> a
foldr1 f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldr1Array call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE foldi #-}
foldi :: ( LikeJSArray tt t
           , LikeJS ta a)
        => (Int -> a -> ArrayElem t -> a)
        -> a -> t -> a
foldi f x0 arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \r e i -> asJSVal (f (asLikeJS i) (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldlArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE foldi1 #-}
foldi1 :: ( LikeJSArray tt t
            , LikeJS ta a)
         => (Int -> a -> ArrayElem t -> a)
         -> t -> a
foldi1 f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \r e i -> asJSVal (f (asLikeJS i) (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldl1Array call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE foldIO #-}
foldIO :: ( LikeJSArray tt t
            , LikeJS ty y)
         => (y -> ArrayElem t -> IO y)
         -> y -> t -> IO y
foldIO f x0 arr = do
        call <- syncCallbackUnsafeIO2 $ \r e -> fmap asJSVal (f (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldlArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE foldIO_ #-}
foldIO_ :: ( LikeJSArray tt t
             , LikeJS ty y)
          => (y -> ArrayElem t -> IO y)
          -> y -> t -> IO ()
foldIO_ f x0 arr = void $ foldIO f x0 arr

{-# INLINE foldiIO #-}
foldiIO :: ( LikeJSArray tt t
             , LikeJS ty y)
          => (Int -> y -> ArrayElem t -> IO y)
          -> y -> t -> IO y
foldiIO f x0 arr = do
        call <- syncCallbackUnsafeIO3 $ \r e i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldlArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE foldiIO_ #-}
foldiIO_ :: ( LikeJSArray tt t
              , LikeJS ty y)
           => (Int -> y -> ArrayElem t -> IO y)
           -> y -> t -> IO ()
foldiIO_ f x0 arr = void $ foldiIO f x0 arr



{-# RULES
"foldlmap/concat"      forall f g x0 arr . foldl  f x0 (map g arr)     = foldl  (\r   -> f r . g  ) x0 arr
"foldlmapSame/concat"  forall f g x0 arr . foldl  f x0 (mapSame g arr) = foldl  (\r   -> f r . g  ) x0 arr
"foldl1map/concat"     forall f g arr    . foldl1 f    (map g arr)     = foldl1 (\r   -> f r . g  )    arr
"foldl1mapSame/concat" forall f g arr    . foldl1 f    (mapSame g arr) = foldl1 (\r   -> f r . g  )    arr
"foldrmap/concat"      forall f g x0 arr . foldr  f x0 (map g arr)     = foldr  (\e r -> f r (g e)) x0 arr
"foldrmapSame/concat"  forall f g x0 arr . foldr  f x0 (mapSame g arr) = foldr  (\e r -> f r (g e)) x0 arr
"foldr1map/concat"     forall f g arr    . foldr1 f    (map g arr)     = foldr1 (\e r -> f r (g e))    arr
"foldr1mapSame/concat" forall f g arr    . foldr1 f    (mapSame g arr) = foldr1 (\e r -> f r (g e))    arr
    #-}


-- zipping

{-# NOINLINE zip #-}
zip :: ( LikeJSArray ta a
         , LikeJSArray tb b
         , LikeJS ty y)
      => (ArrayElem a -> ArrayElem b -> y)
      -> a -> b -> Array y
zip f arr1 arr2 = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \e1 e2 -> asJSVal (f (asLikeJS e1) (asLikeJS e2))
        r <- js_zipArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# NOINLINE zipi #-}
zipi :: ( LikeJSArray ta a
         , LikeJSArray tb b
         , LikeJS ty y)
      => (Int -> ArrayElem a -> ArrayElem b -> y)
      -> a -> b -> Array y
zipi f arr1 arr2 = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \e1 e2 i -> asJSVal (f (asLikeJS i) (asLikeJS e1) (asLikeJS e2))
        r <- js_zipArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE zipIO #-}
zipIO :: ( LikeJSArray ta a
         , LikeJSArray tb b
         , LikeJS ty y)
      => (ArrayElem a -> ArrayElem b -> IO y)
      -> a -> b -> IO (Array y)
zipIO f arr1 arr2 = do
        call <- syncCallbackUnsafeIO2 $ \e1 e2 -> fmap asJSVal (f (asLikeJS e1) (asLikeJS e2))
        r <- js_zipArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE zipiIO #-}
zipiIO :: ( LikeJSArray ta a
         , LikeJSArray tb b
         , LikeJS ty y)
      => (Int -> ArrayElem a -> ArrayElem b -> IO y)
      -> a -> b -> IO (Array y)
zipiIO f arr1 arr2 = do
        call <- syncCallbackUnsafeIO3 $ \e1 e2 i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e1) (asLikeJS e2))
        r <- js_zipArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE zipIO_ #-}
zipIO_ :: ( LikeJSArray ta a
            , LikeJSArray tb b )
         => (ArrayElem a -> ArrayElem b -> IO ())
         -> a -> b -> IO ()
zipIO_ f arr1 arr2 = do
        call <- syncCallback2 ContinueAsync $ \e1 e2 -> f (asLikeJS e1) (asLikeJS e2)
        js_zipArray_ call (toJSArray arr1) (toJSArray arr2)
        releaseCallback call

{-# INLINE zipiIO_ #-}
zipiIO_ :: ( LikeJSArray ta a
             , LikeJSArray tb b )
          => (Int -> ArrayElem a -> ArrayElem b -> IO ())
          -> a -> b -> IO ()
zipiIO_ f arr1 arr2 = do
        call <- syncCallback3 ContinueAsync $ \e1 e2 i -> f (asLikeJS i) (asLikeJS e1) (asLikeJS e2)
        js_zipArray_ call (toJSArray arr1) (toJSArray arr2)
        releaseCallback call

{-# NOINLINE unionZip #-}
unionZip :: ( LikeJSArray ta a
              , LikeJSArray tb b
              , LikeJS ty y )
           => (Int -> Maybe (ArrayElem a) -> Maybe (ArrayElem b) -> y)
           -> a -> b -> Array y
unionZip f arr1 arr2 = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \e1 e2 i -> asJSVal (f (asLikeJS i) (asLikeJS e1) (asLikeJS e2))
        r <- js_unionZipArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE unionZipIO #-}
unionZipIO :: ( LikeJSArray ta a
                , LikeJSArray tb b
                , LikeJS ty y )
             => (Int -> Maybe (ArrayElem a) -> Maybe (ArrayElem b) -> IO y)
             -> a -> b -> IO (Array y)
unionZipIO f arr1 arr2 = do
        call <- syncCallbackUnsafeIO3 $ \e1 e2 i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e1) (asLikeJS e2))
        r <- js_unionZipArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r


{-# INLINE unionZipIO_ #-}
unionZipIO_ :: ( LikeJSArray ta a
                 , LikeJSArray tb b )
              => (Int -> Maybe (ArrayElem a) -> Maybe (ArrayElem b) -> IO ())
              -> a -> b -> IO ()
unionZipIO_ f arr1 arr2 = do
        call <- syncCallback3 ContinueAsync $ \e1 e2 i -> f (asLikeJS i) (asLikeJS e1) (asLikeJS e2)
        js_unionZipArray_ call (toJSArray arr1) (toJSArray arr2)
        releaseCallback call


instance (Show a, LikeJS ta a) => Show (Array a) where
    show = unpack' . js_show . map (pack . show)

{-# RULES
"show/JSStringArray" show = unpack' . js_show
    #-}


-- | Sort an array using JavaScript sort function
foreign import javascript unsafe "$1.sort()"
    sort :: Array a -> Array a

-- | Remove sequential duplicate elements.
--   This returns a list of unique values if was used on a previously sorted array.
foreign import javascript unsafe "$1.map(function(e,i){if(e==$1[i+1]){return null;}else{return e;}}).filter(function(e){return e!=null;})"
    removeSeqDups :: Array a -> Array a

-- mapping

foreign import javascript unsafe "$2.map(h$retIfDef($1))"
    js_mapArray :: Callback f -> Array a -> IO (Array b)


foreign import javascript unsafe "$2.forEach(h$doIfDef($1))"
    js_mapArray_ :: Callback f -> Array a -> IO ()


-- folding

foreign import javascript unsafe "$3.reduce(h$retIfDef2oa($1),$2)"
    js_foldlArray :: Callback f -> JSVal -> Array a -> IO JSVal


foreign import javascript unsafe "$2.reduce(h$retIfDef2oa($1))"
    js_foldl1Array :: Callback f -> Array a -> IO JSVal


foreign import javascript unsafe "$3.reduceRight(h$retIfDef2oa($1),$2)"
    js_foldrArray :: Callback f -> JSVal -> Array a -> IO JSVal


foreign import javascript unsafe "$2.reduceRight(h$retIfDef2oa($1))"
    js_foldr1Array :: Callback f -> Array a -> IO JSVal


-- zipping


foreign import javascript unsafe "if($3){var f = h$retIfDef2($1); $r = $2.map(function(e,i){return f(e,$3[i],i);});}else{$r = [];}"
    js_zipArray :: Callback f -> Array a -> Array b -> IO (Array c)


foreign import javascript unsafe "if($3){var f = h$retIfDef2($1); $2.forEach(function(e,i){f(e,$3[i],i);});}"
    js_zipArray_ :: Callback f -> Array a -> Array b -> IO ()


foreign import javascript unsafe "var le = $2 || [], ri = $3 || []; var n = Math.max(le.length, ri.length); $r = new Array(n); for(var i = 0; i < n; i++){$r[i] = $1(le[i],ri[i],i);}"
    js_unionZipArray :: Callback f -> Array a -> Array b -> IO (Array c)


foreign import javascript unsafe "var le = $2 || [], ri = $3 || []; var n = Math.max(le.length, ri.length); for(var i = 0; i < n; i++){$1(le[i],ri[i],i);}"
    js_unionZipArray_ :: Callback f -> Array a -> Array b -> IO ()

-- filtering

{-# NOINLINE filter #-}
filter :: ( LikeJSArray ta a )
         => (ArrayElem a -> Bool)
         -> a -> a
filter f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
        r <- fromJSArray <$> js_filter call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE mapEither #-}
mapEither :: ( LikeJSArray ta a
               , LikeJS tx x
               , LikeJS ty y)
            => (ArrayElem a -> Either x y)
            -> a -> (Array x, Array y)
mapEither f arr = unsafePerformIO $ do
    call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
    r <- call `seq` js_mapEither call (toJSArray arr)
    r `seq` releaseCallback call
    return r


foreign import javascript unsafe "$2.filter($1)"
    js_filter :: Callback (a -> Bool) -> Array a -> IO (Array a)


foreign import javascript unsafe "var rez = $2.map($1); $r1 = rez.filter(function(e){return !e.isRight();}).map(function(e){return e.left;}); $r2 = rez.filter(function(e){return e.isRight();}).map(function(e){return e.right;});"
    js_mapEither :: (Callback (a -> Either b c)) -> Array a -> IO (Array b, Array c)


foreign import javascript unsafe "JSON.stringify($1)"
  js_show :: Array a -> JSString



--
--foreign import javascript unsafe "LikeHS.listFromArray($1)"
--  js_ArrayToList :: Array a -> Any
--
--
--foreign import javascript unsafe "LikeHS.listToArray($1)"
--  js_ListToArray :: Any -> Array a


----------------------------------------------------------------------------------------------------
-- custom functions
----------------------------------------------------------------------------------------------------

{-# INLINE length #-}
length :: LikeJSArray ta a => a -> Int
length = js_length . toJSArray


foreign import javascript unsafe "$1.length"
    js_length :: Array a -> Int

-- | index JS array
(!) :: LikeJSArray ta a => a -> Int -> ArrayElem a
(!) arr = asLikeJS . js_index (toJSArray arr)


foreign import javascript unsafe "$1[$2]"
    js_index  :: Array a -> Int -> JSVal

{-# INLINE slice #-}
slice :: LikeJSArray ta a => Int -> Int -> a -> a
slice a b = fromJSArray . js_slice a b . toJSArray

{-# INLINE take #-}
take :: LikeJSArray ta a => Int -> a -> a
take n = fromJSArray . js_slice 0 n . toJSArray

{-# INLINE drop #-}
drop :: LikeJSArray ta a => Int -> a -> a
drop n = fromJSArray . js_slice1 n . toJSArray


foreign import javascript unsafe "$3.slice($1,$2)"
    js_slice :: Int -> Int -> Array a -> Array a


foreign import javascript unsafe "$2.slice($1)"
    js_slice1 :: Int -> Array a -> Array a

-- | Concatenate two JS arrays
{-# INLINE concat #-}
concat :: LikeJSArray ta a => a -> a -> a
concat a = fromJSArray . js_concat (toJSArray a) . toJSArray


foreign import javascript unsafe "$1.concat($2)"
    js_concat :: Array a -> Array a -> Array a

-- | Concatenate array of arrays into single array
foreign import javascript unsafe "[].concat.apply([], $1)"
    join :: Array (Array a) -> Array a

foreign import javascript unsafe "[]"
    emptyArray :: Array a

