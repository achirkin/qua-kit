-----------------------------------------------------------------------------
-- |
-- Module      :  Dispatch
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This file is used only for dispatching all the pages.
-- It is imported only once, in Main.
-- All handlers must be imported in this module.
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dispatch () where

import Yesod

import Foundation
import Handler.Home

mkYesodDispatch "App" resourcesApp
