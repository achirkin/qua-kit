-----------------------------------------------------------------------------
-- |
-- Module      :  Dispatch
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dispatch where

import Yesod

--import Data.Text (Text)
--import qualified Data.Text as Text
import Foundation
import Handler.Download
import Handler.Home
import Handler.Preview
import Yesod.Auth


mkYesodDispatch "App" resourcesApp
