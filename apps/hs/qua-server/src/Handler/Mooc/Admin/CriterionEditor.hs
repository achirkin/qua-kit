{-# RecordWildCards #-}
module Handler.Mooc.Admin.CriterionEditor
    ( getAdminCriterionEditorR
    , postAdminCreateCriterionR
    , getAdminEditCriterionR
    , postAdminEditCriterionR
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

import Handler.Mooc.Admin

getAdminCriterionEditorR :: Handler Html
getAdminCriterionEditorR = postAdminCreateCriterionR

postAdminCreateCriterionR :: Handler Html
postAdminCreateCriterionR = do
    requireAdmin
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
        adminLayout "Welcome to the criterion editor" $ do
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

data EditCriterionData = EditCriterionData
    { editCriterionName :: Maybe Text
    , editCriterionDescription :: Maybe Textarea
    , editCriterionImage :: Maybe FileInfo
    , editCriterionIcon :: Maybe FileInfo
    }

editCriterionForm :: Criterion -> AForm Handler EditCriterionData
editCriterionForm Criterion {..} =
    EditCriterionData <$>
    aopt textField (labeledField "name") (Just $ Just criterionName) <*>
    aopt
        textareaField
        (labeledField "description")
        (Just $ Just $ Textarea criterionDescription) <*>
    aopt fileField (labeledField "image") Nothing <*>
    aopt fileField (labeledField "icon") Nothing

getAdminEditCriterionR :: CriterionId -> Handler Html
getAdminEditCriterionR = postAdminEditCriterionR

postAdminEditCriterionR :: CriterionId -> Handler Html
postAdminEditCriterionR criterionId = do
    criterion <- runDB $ get404 criterionId
    ((res, widget), enctype) <-
        runFormPost $
        renderBootstrap3 BootstrapBasicForm $ editCriterionForm criterion
    case res of
        FormFailure msgs -> showFormError msgs widget enctype
        FormMissing -> showFormWidget widget enctype
        FormSuccess dat@EditCriterionData {..} -> do
            let updaterFrom mdat func field =
                    case mdat of
                        Nothing -> pure Nothing
                        Just n -> do
                            r <- func n
                            pure $ Just $ field =. r
            updaters <-
                catMaybes <$>
                sequence
                    [ updaterFrom editCriterionName pure CriterionName
                    , updaterFrom
                          editCriterionDescription
                          (pure . unTextarea)
                          CriterionDescription
                    , updaterFrom
                          editCriterionImage
                          (\n ->
                               fmap LB.toStrict $
                               runResourceT $ fileSource n $$ CB.sinkLbs)
                          CriterionImage
                    , updaterFrom
                          editCriterionIcon
                          (\n ->
                               fmap (TE.decodeUtf8 . LB.toStrict) $
                               runResourceT $ fileSource n $$ CB.sinkLbs)
                          CriterionIcon
                    ]
            runDB $ update criterionId updaters
            showForm (Just dat) [] widget enctype
  where
    showFormWidget = showForm Nothing []
    showFormError = showForm Nothing
    showForm mr msgs widget enctype = do
        adminLayout "Welcome to the single criterion" $ do
            setTitle "qua-kit - single criterion editor"
            $(widgetFile "mooc/admin/criterion-edit")
