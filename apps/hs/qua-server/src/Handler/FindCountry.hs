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

import Data.Conduit
import Database.Persist.Sql
import qualified Data.Conduit.List as CL

import Yesod
import Foundation
import Model

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text

-- | Default number of results to return
ndef :: Int
ndef = 10

-- | Maximum number of results to return
nmax :: Int
nmax = 100

getFindCountryR :: Handler TypedContent
getFindCountryR = TypedContent typeJson . toContent <$> do
  -- query string
  mquery <- lookupGetParam "q"
  -- maximum number of element to return in JSON result
  n <- (\r -> fromMaybe ndef $ r >>= \m -> case Text.decimal m of
                                             Left _ -> Nothing
                                             Right (v,_) -> Just $ min nmax v
       ) <$> lookupGetParam "n"
  case mquery of
    Nothing -> return $ object []
    Just query -> toJSON <$>
      let sql = "SELECT iso,name FROM country WHERE name LIKE '"
                    `Text.append` query
                    `Text.append` "%'"
          src :: Source Handler (Text,Text)
          src = runDBSource (rawQuery sql []
                            $= CL.map (\(PersistText iso : PersistText name : _ ) -> (iso, name))
                            )
      in (\r -> object [ "q" .= r, "n" .= n]) <$> (src $$ CL.consume)

-- Like runDB, but transforms a Source. See respondSourceDB for an example, practical use case.
