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
    ( debugJSVal, debugAny
    , printJSVal, printAny
    , traceShow
    ) where



import JsHs.Types (JSVal)
import Debug.Trace (traceShow)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)


{-# WARNING debugJSVal "Do not use debug functions in production!" #-}
debugJSVal :: JSVal -> a -> a
debugJSVal v a = v `seq` debugJSVal' v `seq` a

{-# WARNING printJSVal "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" printJSVal :: JSVal -> IO ()

{-# NOINLINE debugAny #-}
{-# WARNING debugAny "Do not use debug functions in production!" #-}
debugAny :: b -> a -> a
debugAny b a = b `seq` debugAny' (unsafeCoerce b) `seq` a

{-# WARNING printAny "Do not use debug functions in production!" #-}
printAny :: a -> IO ()
printAny = printAny' . unsafeCoerce


-------------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------------


{-# WARNING printAny' "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" printAny' :: Any -> IO ()

{-# NOINLINE debugJSVal' #-}
{-# WARNING debugJSVal' "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" debugJSVal' :: JSVal -> ()

{-# NOINLINE debugAny' #-}
{-# WARNING debugAny' "Do not use debug functions in production!" #-}
foreign import javascript unsafe "console.log($1)" debugAny' :: Any -> ()
