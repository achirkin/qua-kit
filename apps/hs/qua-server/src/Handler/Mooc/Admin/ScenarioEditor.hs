{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Mooc.Admin.ScenarioEditor
    ( getAdminScenarioEditorR
    , postAdminCreateScenarioR
    , getExerciseImgR
    , getExerciseGeometryR
    , getExerciseEditR
    , postExerciseAttachCriterionR
    , postExerciseDetachCriterionR
    ) where

import Import hiding ((/=.), (==.), isNothing, on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head)
import qualified Data.Text as T

import Database.Esqueleto
import qualified Database.Persist as P
import System.Random

import Yesod.Auth.Email
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
            invitationSecret <- liftIO generateInvitationSecret
            runDB $
                insert_
                    Exercise
                    { exerciseDescription = newScenarioDataDescription
                    , exerciseImage = imageBs
                    , exerciseGeometry = geometryBs
                    , exerciseScale = newScenarioDataScale
                    , exerciseCanAddDeleteGeom = False
                    , exerciseInvitationSecret = invitationSecret
                    }
            showForm (Just dat) [] widget enctype
  where
    showFormWidget = showForm Nothing []
    showFormError = showForm Nothing
    showForm ::
           Maybe NewScenarioData
        -> [Text]
        -> WidgetT App IO ()
        -> Enctype
        -> HandlerT App IO Html
    showForm mr msgs widget enctype = do
        scenarioWidgets <- getScenarioCards
        adminLayout "Welcome to the scenario editor" $ do
            setTitle "qua-kit - scenario editor"
            $(widgetFile "mooc/admin/scenario-editor")

generateInvitationSecret :: IO Text
generateInvitationSecret = T.pack <$> replicateM 16 (randomRIO ('a', 'z'))

data NewScenarioData = NewScenarioData
    { newScenarioDataDescription :: Text
    , newScenarioDataImage :: FileInfo
    , newScenarioDataScale :: Double
    , newScenarioDataGeometry :: FileInfo
    }

newScenarioForm :: AForm Handler NewScenarioData
newScenarioForm =
    NewScenarioData <$> areq textField (labeledField "description") Nothing <*>
    areq fileField (labeledField "image") Nothing <*>
    areq doubleField (labeledField "scale") (Just 0.5) <*>
    areq fileField (labeledField "geometry") Nothing

labeledField :: Text -> FieldSettings App
labeledField = bfs

getScenarioCards :: Handler [Widget]
getScenarioCards = do
    tups <-
        (runDB $
         select $
         from $ \(LeftOuterJoin (LeftOuterJoin exercise exerciseCriterion) criterion) -> do
             on
                 (exerciseCriterion ?. ExerciseCriterionCriterionId ==. criterion ?.
                  CriterionId)
             on
                 (exerciseCriterion ?. ExerciseCriterionExerciseId ==.
                  just (exercise ^. ExerciseId))
             pure (exercise, criterion)) :: Handler [( Entity Exercise
                                                            , Maybe (Entity Criterion))]
    mapM (uncurry scenarioWidget . second catMaybes) $ groupsOf tups

groupsOf :: Ord a => [(a, b)] -> [(a, [b])]
groupsOf =
    map (\ls -> (fst $ List.head ls, map snd ls)) .
    List.groupBy ((==) `Function.on` fst) . sortOn fst

scenarioWidget ::
       Entity Exercise -> [Entity Criterion] -> Handler Widget
scenarioWidget (Entity exerciseId Exercise {..}) cs = do
    inviteLink <- getInviteLink exerciseId
    pure $(widgetFile "mooc/admin/scenario-card")

getInviteLink :: ExerciseId -> Handler Text
getInviteLink exerciseId = do
    urlRender <- getUrlRenderParams
    params <- invitationParams exerciseId
    pure $ urlRender (AuthR registerR) params

getExerciseImgR :: ExerciseId -> Handler TypedContent
getExerciseImgR exerciseId = do
    scenario <- runDB $ get404 exerciseId
    addHeader "Content-Disposition" "inline"
    sendResponse
        ("image/png" :: ByteString, toContent $ exerciseImage scenario)

getExerciseGeometryR :: ExerciseId -> Handler TypedContent
getExerciseGeometryR exerciseId = do
    scenario <- runDB $ get404 exerciseId
    addHeader "Content-Disposition" "inline"
    sendResponse
        ("text/plain" :: ByteString
        , toContent $ exerciseGeometry scenario)

getExerciseEditR :: ExerciseId -> Handler Html
getExerciseEditR exerciseId = do
    requireAdmin
    Exercise {..} <- runDB $ get404 exerciseId
    cs <-
        runDB $
        select $
        from $ \(LeftOuterJoin criterion exerciseCriterion) -> do
            on
                ((exerciseCriterion ?. ExerciseCriterionCriterionId ==.
                  just (criterion ^. CriterionId)) &&.
                 (exerciseCriterion ?. ExerciseCriterionExerciseId ==.
                  just (val exerciseId)))
            pure
                ( criterion ^. CriterionId
                , criterion ^. CriterionName
                , not_ $ isNothing $ exerciseCriterion ?. ExerciseCriterionId)
    adminLayout
        (T.pack $
         unwords
             [ "Welcome to the editor for scenario"
             , show (fromSqlKey exerciseId) ++ ":"
             , T.unpack exerciseDescription
             ]) $ do
        setTitle "qua-kit - scenario"
        $(widgetFile "mooc/admin/scenario-edit")

postExerciseAttachCriterionR ::
       ExerciseId -> CriterionId -> Handler ()
postExerciseAttachCriterionR s c = do
    void $
        runDB $ do
            mr <-
                selectFirst
                    [ ExerciseCriterionExerciseId P.==. s
                    , ExerciseCriterionCriterionId P.==. c
                    ]
                    []
            case mr of
                Nothing ->
                    insert_
                        ExerciseCriterion
                        { exerciseCriterionExerciseId = s
                        , exerciseCriterionCriterionId = c
                        }
                Just _ -> pure ()
    redirect $ ExerciseEditR s

postExerciseDetachCriterionR ::
       ExerciseId -> CriterionId -> Handler ()
postExerciseDetachCriterionR s c = do
    runDB $
        P.deleteWhere
            [ ExerciseCriterionExerciseId P.==. s
            , ExerciseCriterionCriterionId P.==. c
            ]
    redirect $ ExerciseEditR s
