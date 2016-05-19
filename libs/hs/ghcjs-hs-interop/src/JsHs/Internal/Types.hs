{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.Internal.Types
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- Most of this file is copied from https://github.com/ghcjs/ghcjs
-- licensed under MIT license
--
-----------------------------------------------------------------------------

module JsHs.Internal.Types
    ( MutabilityType (..)
    , IsItMutable (..)
    , Mutability
    ) where



data MutabilityType s = Mutable
                      | Immutable
                      | STMutable s

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability 'Immutable     = 'IsImmutable
  Mutability 'Mutable       = 'IsMutable
  Mutability ('STMutable s) = 'IsMutable
