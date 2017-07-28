{-# OPTIONS_HADDOCK hide, prune #-}
module Import.Util where

import Control.Monad.Trans.Except
import Import

maybeE :: (Monad m) => Text -> m (Maybe a) -> ExceptT Text m a
maybeE errtxt m = ExceptT $ m >>= \mv -> case mv of
  Nothing -> return $ Left errtxt
  Just v  -> return $ Right v

runJSONExceptT :: ExceptT Text Handler Value -> Handler Value
runJSONExceptT m = f <$> runExceptT m
  where
    f (Left err) = object ["error" .= err]
    f (Right v ) = v
