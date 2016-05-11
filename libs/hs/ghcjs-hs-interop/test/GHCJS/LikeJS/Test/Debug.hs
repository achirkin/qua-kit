{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim, MagicHash, BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
module GHCJS.LikeJS.Test.Debug
    ( debugJSVal, debugAny
    , printJSVal, printAny
    , traceShow
    ) where



import GHCJS.Types (JSVal)
import Debug.Trace (traceShow)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)


debugJSVal :: JSVal -> a -> a
debugJSVal v a = v `seq` debugJSVal' v `seq` a

foreign import javascript unsafe "console.log($1)" printJSVal :: JSVal -> IO ()

debugAny :: b -> a -> a
debugAny b a = b `seq` debugAny' (unsafeCoerce b) `seq` a

printAny :: a -> IO ()
printAny = printAny' . unsafeCoerce

foreign import javascript unsafe "console.log($1)" printAny' :: Any -> IO ()
{-# NOINLINE debugJSVal' #-}
foreign import javascript unsafe "console.log($1)" debugJSVal' :: JSVal -> ()
{-# NOINLINE debugAny' #-}
foreign import javascript unsafe "console.log($1)" debugAny' :: Any -> ()
