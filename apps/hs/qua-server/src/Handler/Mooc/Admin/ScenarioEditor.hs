{-# RecordWildCards #-}
module Handler.Mooc.Admin.ScenarioEditor
    ( getAdminScenarioEditorR
    , postAdminCreateScenarioR
    , getScenarioProblemImgR
    , getScenarioProblemGeometryR
    , getScenarioProblemEditR
    , postScenarioProblemAttachCriterionR
    , postScenarioProblemDetachCriterionR
    ) where

import Import hiding ((/=.), (==.), isNothing, on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head, nubBy)
import qualified Data.Text as T

import Database.Esqueleto
import qualified Database.Persist as P

import Yesod.Form.Bootstrap3

import Handler.Mooc.Admin

getAdminScenarioEditorR :: Handler Html
getAdminScenarioEditorR = postAdminCreateScenarioR

postAdminCreateScenarioR :: Handler Html
postAdminCreateScenarioR = do
    requireAdmin
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
        adminLayout "Welcome to the scenario editor" $ do
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
    requireAdmin
    ScenarioProblem {..} <- runDB $ get404 scenarioProblemId
    cs <-
        runDB $
        select $
        from $ \(LeftOuterJoin criterion problemCriterion) -> do
            on
                ((problemCriterion ?. ProblemCriterionCriterionId ==.
                  just (criterion ^. CriterionId)) &&.
                 (problemCriterion ?. ProblemCriterionProblemId ==.
                  just (val scenarioProblemId)))
            pure
                ( criterion ^. CriterionId
                , criterion ^. CriterionName
                , not_ $ isNothing $ problemCriterion ?. ProblemCriterionId)
    adminLayout
        (T.pack $
         unwords
             [ "Welcome to the editor for scenario"
             , show (fromSqlKey scenarioProblemId) ++ ":"
             , T.unpack scenarioProblemDescription
             ]) $ do
        setTitle "qua-kit - scenario"
        $(widgetFile "mooc/admin/scenario-edit")

postScenarioProblemAttachCriterionR ::
       ScenarioProblemId -> CriterionId -> Handler ()
postScenarioProblemAttachCriterionR s c = do
    void $
        runDB $ do
            mr <-
                selectFirst
                    [ ProblemCriterionProblemId P.==. s
                    , ProblemCriterionCriterionId P.==. c
                    ]
                    []
            case mr of
                Nothing ->
                    insert_
                        ProblemCriterion
                        { problemCriterionProblemId = s
                        , problemCriterionCriterionId = c
                        }
                Just _ -> pure ()
    redirect $ ScenarioProblemEditR s

postScenarioProblemDetachCriterionR ::
       ScenarioProblemId -> CriterionId -> Handler ()
postScenarioProblemDetachCriterionR s c = do
    runDB $
        P.deleteWhere
            [ ProblemCriterionProblemId P.==. s
            , ProblemCriterionCriterionId P.==. c
            ]
    redirect $ ScenarioProblemEditR s
