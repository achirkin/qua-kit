-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.FindCountry
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Handler.FindCountry
    (getFindCountryR
    ) where

import Yesod
import Foundation
import Model


getFindCountryR :: Handler TypedContent
getFindCountryR = TypedContent typeJson . toContent <$> do
  mquery <- lookupGetParam "q"
  case mquery of
    Nothing -> return $ object []
    Just query -> returnJson $ [query]
