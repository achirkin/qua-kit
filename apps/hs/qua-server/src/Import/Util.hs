{-# OPTIONS_HADDOCK hide, prune #-}
module Import.Util where

import Control.Monad.Trans.Except
import Import

maybeE :: (Monad m) => Text -> m (Maybe a) -> ExceptT Text m a
maybeE errtxt m = ExceptT $ m >>= \mv -> case mv of
  Nothing -> return $ Left errtxt
  Just v  -> return $ Right v

runJSONExceptT :: (ToJSON a)
               => ExceptT Text Handler a -> Handler Value
runJSONExceptT m = runExceptT m >>= f
  where
    f (Left err) = sendResponseStatus badRequest400 $ object [ "message" .= err ]
    f (Right v ) = returnJson v
