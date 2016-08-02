{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.Callback
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module JsHs.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
      -- * asynchronous callbacks
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
      -- * synchronous callbacks
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
      -- * synchronous callbacks that return a value
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
      -- * Really unsafe synchronous callbacks that could not be interrupted in any way
    , syncCallbackUnsafe1
    , syncCallbackUnsafe2
    , syncCallbackUnsafe3
    , syncCallbackUnsafeIO1
    , syncCallbackUnsafeIO2
    , syncCallbackUnsafeIO3
    ) where

import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (Any)

import GHCJS.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
    )

import JsHs.Types (JSVal)


{-# INLINE syncCallbackUnsafe1 #-}
syncCallbackUnsafe1 :: (JSVal -> JSVal) -> IO (Callback f)
syncCallbackUnsafe1 x = js_syncCallbackApplyReturnUnsafe 1 (unsafeCoerce x)
{-# INLINE syncCallbackUnsafeIO1 #-}
syncCallbackUnsafeIO1 :: (JSVal -> IO JSVal) -> IO (Callback f)
syncCallbackUnsafeIO1 x = js_syncCallbackApplyReturnUnsafe 1 (unsafeCoerce x)

{-# INLINE syncCallbackUnsafe2 #-}
syncCallbackUnsafe2 :: (JSVal -> JSVal -> JSVal) -> IO (Callback f)
syncCallbackUnsafe2 x = js_syncCallbackApplyReturnUnsafe 2 (unsafeCoerce x)
{-# INLINE syncCallbackUnsafeIO2 #-}
syncCallbackUnsafeIO2 :: (JSVal -> JSVal -> IO JSVal) -> IO (Callback f)
syncCallbackUnsafeIO2 x = js_syncCallbackApplyReturnUnsafe 2 (unsafeCoerce x)

{-# INLINE syncCallbackUnsafe3 #-}
syncCallbackUnsafe3 :: (JSVal -> JSVal -> JSVal -> JSVal) -> IO (Callback f)
syncCallbackUnsafe3 x = js_syncCallbackApplyReturnUnsafe 3 (unsafeCoerce x)
{-# INLINE syncCallbackUnsafeIO3 #-}
syncCallbackUnsafeIO3 :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO (Callback f)
syncCallbackUnsafeIO3 x = js_syncCallbackApplyReturnUnsafe 3 (unsafeCoerce x)


foreign import javascript unsafe
  "h$makeCallbackApply($1, h$runSyncReturnUnsafe, [false], $2)"
  js_syncCallbackApplyReturnUnsafe :: Int -> Any -> IO (Callback f)
