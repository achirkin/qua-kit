{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim, MagicHash, BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.Debug
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- A set of hackery GHCJS function often used for debugging
--
-----------------------------------------------------------------------------
module JsHs.Debug
    ( traceJSVal, traceAny, traceShow
    , printJSVal, printAny
    ) where



import JsHs.Types (JSVal)
import Debug.Trace (traceShow)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)

{-# NOINLINE traceJSVal #-}
{-# WARNING traceJSVal "Do not use debug functions in production!" #-}
traceJSVal :: JSVal -> a -> a
traceJSVal v a = v `seq` debugJSVal' v `seq` a

{-# WARNING printJSVal "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" printJSVal :: JSVal -> IO ()

{-# NOINLINE traceAny #-}
{-# WARNING traceAny "Do not use debug functions in production!" #-}
traceAny :: b -> a -> a
traceAny b a = b `seq` debugAny' (unsafeCoerce b) `seq` a

{-# WARNING printAny "Do not use debug functions in production!" #-}
printAny :: a -> IO ()
printAny = printAny' . unsafeCoerce


-------------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------------


{-# WARNING printAny' "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" printAny' :: Any -> IO ()

{-# WARNING debugJSVal' "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" debugJSVal' :: JSVal -> ()

{-# WARNING debugAny' "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" debugAny' :: Any -> ()
