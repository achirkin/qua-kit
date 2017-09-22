-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Helen.Core
import Helen.Core.OptParse
import Helen.Core.Types

main :: IO ()
main = do
    sets <- getSettings
    putStrLn $ "Helen is running on port " ++ show (setPort sets)
    helen <- initHelen sets
    _ <- runHelenProgram helen program
    putStrLn "Helen finished."
