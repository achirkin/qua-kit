{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Mooc.Admin.ExerciseEditor
    ( getAdminExerciseEditorR
    , postAdminCreateExerciseR
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
import Text.Blaze.Html.Renderer.Text
import Text.RE.TDFA.Text

import Database.Esqueleto
import qualified Database.Persist as P
import System.Random

import Yesod.Auth.Email
import Yesod.Form.Bootstrap3

import Handler.Mooc.Admin

getAdminExerciseEditorR :: Handler Html
getAdminExerciseEditorR = postAdminCreateExerciseR

postAdminCreateExerciseR :: Handler Html
postAdminCreateExerciseR = do
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
                    , exerciseCanAddDeleteGeom = newScenarioDataCanAddDeleteGeom
                    , exerciseInvitationSecret = invitationSecret
                    , exerciseOnSubmitMsg
                        = (*=~/ [edBS|(<a[^>]*)>///$1 onclick="window.open(this.href)" target="_blank">|])
                        . (*=~/ [edBS|onclick=\"[^\"]*\"///|])
                        . (*=~/ [edBS|target=\"[^\"]*\"///|])
                        . toStrict $ renderHtml $ newScenarioDataOnSubmitMessage
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
        adminLayout "Welcome to the exercise editor" $ do
            setTitle "qua-kit - exercise editor"
            $(widgetFile "mooc/admin/exercise-editor")

generateInvitationSecret :: IO Text
generateInvitationSecret = T.pack <$> replicateM 16 (randomRIO ('a', 'z'))

data NewScenarioData = NewScenarioData
    { newScenarioDataDescription :: Text
    , newScenarioDataImage :: FileInfo
    , newScenarioDataScale :: Double
    , newScenarioDataGeometry :: FileInfo
    , newScenarioDataCanAddDeleteGeom :: Bool
    , newScenarioDataOnSubmitMessage :: Html
    }

newScenarioForm :: AForm Handler NewScenarioData
newScenarioForm =
    NewScenarioData <$> areq textField (labeledField "description") Nothing <*>
    areq fileField (labeledField "image") Nothing <*>
    areq doubleField (labeledField "scale (obsolete)") (Just 0.5) <*>
    areq fileField (labeledField "geometry") Nothing <*>
    areq boolField (labeledField "Allow students to add/delete objects.") (Just False) <*>
    areq htmlField (labeledField "On-submit html message. Use ${userId}, ${userName}, and ${exerciseId} to customize it.")
       (Just
          [shamlet|
             <h5>Thank you, ${userName}!
             <p>Your design submission has been saved.
                Though you can continue working on it and re-submit it later.
             <a href="https://httpbin.org/get?userId=${userId}&userName=${userName}&exId=${exerciseId}">
                Proceed with a personalized link
          |]
       )

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
