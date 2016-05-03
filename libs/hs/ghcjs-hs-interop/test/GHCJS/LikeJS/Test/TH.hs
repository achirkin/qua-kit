{-# LANGUAGE TemplateHaskell #-}
module GHCJS.LikeJS.Test.TH (writeTests) where

import Control.Arrow (second)
import Test.Framework

import Language.Haskell.TH
import Control.Monad (join)
import Data.Maybe (catMaybes)
--import Debug.Trace (traceShow)



-- | Write specified test suite on specified types
--   Example:
--     $(writeTests "likeJSConversion"
--       [ ''Int, ''Float, ''Double, ''Bool, ''Char]
--          (\t ->
--              [d| prop_doubleConversion :: $(t) -> Bool
--                  prop_doubleConversion = doubleConversion
--              |]
--          )
--      )
writeTests :: String -- ^ name of the test suite  (:: TestSuite)
           -> [Name] -- ^ names of rank-1 types to generate tests on
           -> (TypeQ -> Q [Dec]) -- ^ a splice that generate a test, its parameter is a type constructor
           -> Q [Dec]
writeTests sName tnames test = do
    locTH <- location
    (tdecs, fnames) <- appliedTests
    let -- name of the module
        moduleName = loc_module locTH
        -- Location of the splice in HTF format
        loc = VarE 'makeLoc
                   `AppE` (LitE . StringL $ loc_filename locTH)
                   `AppE` (LitE . IntegerL . fromIntegral . fst $ loc_start locTH)
        -- generate pairs of function (string) names and functions theirselves (IO ())
        gennames :: ExpQ
        gennames = pure
                 . ListE
                 . map (\gn -> TupE [ LitE . StringL $ nameBase gn
                                    , VarE 'qcAssertion `AppE` VarE gn])
                 $ fnames
    testSuiteImpl <- (\e -> FunD (mkName sName) [Clause [] (NormalB e) []] ) <$>
      [e| makeTestSuite
            $(return . LitE . StringL $ moduleName ++ '.':sName)
            . map (\(fname,ffunc) -> makeQuickCheckTest fname $(return loc) ffunc)
            $ $(gennames)
        |]

    return $ testSuiteDec : testSuiteImpl : tdecs
  where
    testSuiteDec = SigD (mkName sName) (ConT ''TestSuite)
    appliedTests :: Q ([Dec], [Name])
    appliedTests = second catMaybes
                 . unzip
                 . join
                 <$>
                  (mapM
                    (\name -> renameFuncSuf (nameBase name) <$> test (pure $ ConT name))
                    tnames
                  )


-- | Append suffix to the names of the functions
renameFuncSuf :: String -> [Dec] -> [(Dec, Maybe Name)]
renameFuncSuf suffix = map f
    where appendSuf name = mkName $ nameBase name ++ suffix
          f (SigD name sig) = (SigD (appendSuf name) sig, Nothing )
          f (FunD name fun) = let s = appendSuf name
                              in (FunD s fun, Just s)
          f (ValD (VarP name) fun decs) = let s = appendSuf name
                                          in (ValD (VarP s) fun decs, Just s)
          f x = (x, Nothing)
