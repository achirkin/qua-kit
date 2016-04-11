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

import Foundation
import Handler.Image
import Handler.ImgPreview
import Handler.Home
import Handler.Preview
import Handler.ImageUpload
import Handler.FindCountry
import Handler.FindPlace



mkYesodDispatch "App" resourcesApp
