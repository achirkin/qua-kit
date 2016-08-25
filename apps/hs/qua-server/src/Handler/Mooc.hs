-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Handler.Mooc
  ( getMoocHomeR, postMoocHomeR
  ) where





import qualified Data.Text as Text
import qualified Data.Map as Map
--import qualified Database.Esqueleto      as E
--import           Database.Esqueleto      ((^.))
--import qualified Database.Persist.Sql    as PSQL

import Import


postMoocHomeR :: Handler Html
postMoocHomeR = getMoocHomeR

getMoocHomeR :: Handler Html
getMoocHomeR  = do
    setUltDestCurrent

    mmsg <- getMessage
    muser <- fmap entityVal <$> maybeAuth

    ses <- map (\(k,v) -> k <> " - " <> decodeUtf8 v) . Map.toList <$> getSession

    defaultLayout $ do
        setTitle "EdX User Stories"
        $(widgetFile "mooc/home")



-- | Get two capital characters out of a name
twoCharsName :: Text -> Text
twoCharsName s = case filter (not . null) $ Text.words s of
   [name] -> Text.toUpper $ Text.take 2 name
   n1:n2:_ -> Text.toUpper $ Text.take 1 n1 <> Text.take 1 n2
   _ -> "??"
