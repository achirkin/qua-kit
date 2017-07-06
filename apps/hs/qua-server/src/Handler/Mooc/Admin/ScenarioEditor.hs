{-# RecordWildCards #-}
module Handler.Mooc.Admin.ScenarioEditor
    ( getAdminScenarioEditorR
    , postAdminCreateScenarioR
    , getScenarioProblemImgR
    , getScenarioProblemGeometryR
    , getScenarioProblemEditR
    ) where

import Import hiding ((==.), on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head)
import qualified Data.Text as T

import Database.Esqueleto
import qualified Database.Persist as P

import Yesod.Form.Bootstrap3

getAdminScenarioEditorR :: Handler Html
getAdminScenarioEditorR = postAdminCreateScenarioR

postAdminCreateScenarioR :: Handler Html
postAdminCreateScenarioR = do
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
    showForm mr msgs widget enctype = do
        scenarioWidgets <- getScenarioCards
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

getScenarioCards :: Handler [Widget]
getScenarioCards = do
    tups <-
        (runDB $
         select $
         from $ \(LeftOuterJoin (LeftOuterJoin scenarioProblem problemCriterion) criterion) -> do
             on
                 (problemCriterion ?. ProblemCriterionCriterionId ==. criterion ?.
                  CriterionId)
             on
                 (problemCriterion ?. ProblemCriterionProblemId ==.
                  just (scenarioProblem ^. ScenarioProblemId))
             pure (scenarioProblem, criterion)) :: Handler [( Entity ScenarioProblem
                                                            , Maybe (Entity Criterion))]
    pure $ map (uncurry scenarioWidget) $ map (second catMaybes) $ groupsOf tups

groupsOf :: Ord a => [(a, b)] -> [(a, [b])]
groupsOf =
    map (\ls -> (fst $ List.head ls, map snd ls)) .
    List.groupBy ((==) `Function.on` fst) . sortOn fst

scenarioWidget :: Entity ScenarioProblem -> [Entity Criterion] -> Widget
scenarioWidget (Entity scenarioProblemId ScenarioProblem {..}) cs =
    $(widgetFile "mooc/admin/scenario-card")

getScenarioProblemImgR :: ScenarioProblemId -> Handler TypedContent
getScenarioProblemImgR scenarioProblemId = do
    scenario <- runDB $ get404 scenarioProblemId
    addHeader "Content-Disposition" "inline"
    sendResponse
        (("image/png" :: ByteString), toContent $ scenarioProblemImage scenario)

getScenarioProblemGeometryR :: ScenarioProblemId -> Handler TypedContent
getScenarioProblemGeometryR scenarioProblemId = do
    scenario <- runDB $ get404 scenarioProblemId
    addHeader "Content-Disposition" "inline"
    sendResponse
        ( ("text/plain" :: ByteString)
        , toContent $ scenarioProblemGeometry scenario)

getScenarioProblemEditR :: ScenarioProblemId -> Handler Html
getScenarioProblemEditR scenarioProblemId = do
    ScenarioProblem {..} <- runDB $ get404 scenarioProblemId
    cs <-
            runDB $
              select $
              from $ \(InnerJoin problemCriterion criterion) -> do
                  on
                      (problemCriterion ^. ProblemCriterionCriterionId ==.
                       criterion ^.
                       CriterionId)
                  where_
                      (problemCriterion ^. ProblemCriterionProblemId ==.
                       val scenarioProblemId)
                  pure criterion
    fullLayout
        Nothing
        (T.pack $
         unwords
             [ "Welcome to the editor for scenario"
             , show (fromSqlKey scenarioProblemId) ++ ":"
             , T.unpack scenarioProblemDescription
             ]) $ do
        setTitle "qua-kit - scenario"
        $(widgetFile "mooc/admin/scenario-edit")
