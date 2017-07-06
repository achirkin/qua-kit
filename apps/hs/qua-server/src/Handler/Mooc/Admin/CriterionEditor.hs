{-# RecordWildCards #-}
module Handler.Mooc.Admin.CriterionEditor
    ( getAdminCriterionEditorR
    , postAdminCreateCriterionR
    , getCriterionProblemEditR
    ) where

import Import hiding ((==.), on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Blaze as Blaze

import Database.Persist

import Yesod.Form.Bootstrap3

getAdminCriterionEditorR :: Handler Html
getAdminCriterionEditorR = postAdminCreateCriterionR

postAdminCreateCriterionR :: Handler Html
postAdminCreateCriterionR = do
    ((res, widget), enctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm newCriterionForm
    case res of
        FormFailure msgs -> showFormError msgs widget enctype
        FormMissing -> showFormWidget widget enctype
        FormSuccess dat@NewCriterionData {..} -> do
            imageBs <-
                fmap LB.toStrict $
                runResourceT $ fileSource newCriterionDataImage $$ CB.sinkLbs
            iconText <-
                fmap (TE.decodeUtf8 . LB.toStrict) $
                runResourceT $ fileSource newCriterionDataIcon $$ CB.sinkLbs
            runDB $
                insert_
                    Criterion
                    { criterionName = newCriterionDataName
                    , criterionDescription = newCriterionDataDescription
                    , criterionImage = imageBs
                    , criterionIcon = iconText
                    }
            showForm (Just dat) Nothing widget enctype
  where
    showFormWidget = showForm Nothing []
    showFormError = showForm Nothing
    showForm mr msgs widget enctype = do
        criterionWidgets <- getCriterionCards
        fullLayout Nothing "Welcome to the criterion editor" $ do
            setTitle "qua-kit - criterion editor"
            $(widgetFile "mooc/admin/criterion-editor")

data NewCriterionData = NewCriterionData
    { newCriterionDataName :: Text
    , newCriterionDataDescription :: Text
    , newCriterionDataImage :: FileInfo
    , newCriterionDataIcon :: FileInfo
    }

newCriterionForm :: AForm Handler NewCriterionData
newCriterionForm =
    NewCriterionData <$> areq textField (labeledField "name") Nothing <*>
    areq textField (labeledField "description") Nothing <*>
    areq fileField (labeledField "image") Nothing <*>
    areq fileField (labeledField "icon") Nothing

labeledField :: Text -> FieldSettings App
labeledField t = bfs t

getCriterionCards :: Handler [Widget]
getCriterionCards = do
    criterion <- runDB $ selectList [] [Asc CriterionId]
    pure $ map criterionWidget criterion

criterionWidget :: Entity Criterion -> Widget
criterionWidget (Entity criterionId Criterion {..}) =
    $(widgetFile "mooc/admin/criterion-card")

getCriterionProblemEditR :: CriterionId -> Handler Html
getCriterionProblemEditR criterionId = pure mempty
   --  requireAdmin
   --  CriterionProblem {..} <- runDB $ get404 scenarioProblemId
   --  cs <-
   --      runDB $
   --      select $
   --      from $ \(InnerJoin problemCriterion criterion) -> do
   --          on
   --              (problemCriterion ^. ProblemCriterionCriterionId ==. criterion ^.
   --               CriterionId)
   --          where_
   --              (problemCriterion ^. ProblemCriterionProblemId ==.
   --               val scenarioProblemId)
   --          pure criterion
   --  fullLayout
   --      Nothing
   --      (T.pack $
   --       unwords
   --           [ "Welcome to the editor for scenario"
   --           , show (fromSqlKey scenarioProblemId) ++ ":"
   --           , T.unpack scenarioProblemDescription
   --           ]) $ do
   --      setTitle "qua-kit - scenario"
   --      $(widgetFile "mooc/admin/scenario-edit")
