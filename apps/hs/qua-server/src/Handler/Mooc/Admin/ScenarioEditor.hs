{-# RecordWildCards #-}
module Handler.Mooc.Admin.ScenarioEditor
    ( getAdminScenarioEditor
    , postAdminCreateScenario
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB

import Yesod.Form.Bootstrap3

getAdminScenarioEditor :: Handler Html
getAdminScenarioEditor = postAdminCreateScenario

postAdminCreateScenario :: Handler Html
postAdminCreateScenario = do
    ((res, widget), enctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm newScenarioForm
    case res of
        FormFailure msgs -> showFormError msgs widget enctype
        FormMissing -> showFormWidget widget enctype
        FormSuccess dat@NewScenarioData {..} -> do
            imageBs <-
                fmap LB.toStrict $
                runResourceT $ fileSource newScenarioDataImage $$ CB.sinkLbs
            geometryBs <-
                fmap LB.toStrict $
                runResourceT $ fileSource newScenarioDataGeometry $$ CB.sinkLbs
            runDB $
                insert_
                    ScenarioProblem
                    { scenarioProblemDescription = newScenarioDataDescription
                    , scenarioProblemImage = imageBs
                    , scenarioProblemGeometry = geometryBs
                    , scenarioProblemScale = 1
                    }
            showForm (Just dat) Nothing widget enctype
  where
    showFormWidget = showForm Nothing []
    showFormError = showForm Nothing
    showForm mr msgs widget enctype =
        fullLayout Nothing "Welcome to the scenario editor" $ do
            setTitle "qua-kit - scenario editor"
            $(widgetFile "mooc/admin/scenario-editor")

data NewScenarioData = NewScenarioData
    { newScenarioDataDescription :: Text
    , newScenarioDataImage :: FileInfo
    , newScenarioDataGeometry :: FileInfo
    }

newScenarioForm :: AForm Handler NewScenarioData
newScenarioForm =
    NewScenarioData <$> areq textField (labeledField "description") Nothing <*>
    areq fileField (labeledField "image") Nothing <*>
    areq fileField (labeledField "geometry") Nothing

labeledField :: Text -> FieldSettings App
labeledField t = bfs t
