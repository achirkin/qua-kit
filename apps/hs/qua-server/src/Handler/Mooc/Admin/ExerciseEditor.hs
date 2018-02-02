{-# OPTIONS_HADDOCK hide, prune #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Mooc.Admin.ExerciseEditor
    ( getAdminExerciseEditorR
    , getExerciseEditR
    , postAdminExercisesR
    , putAdminExerciseR
    , getExerciseImgR
    , getExerciseGeometryR
    , postExerciseAttachCriterionR
    , postExerciseDetachCriterionR
    ) where

import Import hiding ((/=.), (==.), isNothing, on)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Binary as CB
import qualified Data.Function as Function (on)
import qualified Data.List as List (groupBy, head)
import qualified Data.Text as T
import Text.Blaze (preEscapedText)
import Text.Blaze.Html.Renderer.Text
import Text.RE.TDFA.Text

import Database.Esqueleto
import qualified Database.Persist as P
import System.Random

import Yesod.Auth.Email
import Yesod.Form.Bootstrap3

import Handler.Mooc.Admin

getAdminExerciseEditorR :: Handler Html
getAdminExerciseEditorR = postAdminExercisesR

putAdminExerciseR :: ExerciseId -> Handler Html
putAdminExerciseR = createOrUpdateExercise . Just

postAdminExercisesR :: Handler Html
postAdminExercisesR = createOrUpdateExercise Nothing

createOrUpdateExercise :: Maybe ExerciseId -> Handler Html
createOrUpdateExercise mExId = do
  requireAdmin
  ((res, widget), enctype) <-
      runFormPost $ renderBootstrap3 BootstrapBasicForm $ exerciseForm Nothing
  case res of
      FormFailure msgs -> showFormError msgs widget enctype
      FormMissing -> showFormWidget widget enctype
      FormSuccess dat@ExerciseData {..} -> do
        mImg <- case newScenarioDataImage of
                  Just img -> getBs img
                  Nothing -> return Nothing
        mGeo <- case newScenarioDataGeometry of
                  Just geo -> getBs geo
                  Nothing -> return Nothing
        case mExId of
          Just exId -> do
            --update
            void $ runDB $ P.update exId $ [
                ExerciseDescription       P.=. newScenarioDataDescription
              , ExerciseScale             P.=. newScenarioDataScale
              , ExerciseCanAddDeleteGeom  P.=. newScenarioDataCanAddDeleteGeom
              , ExerciseCanEditProperties P.=. newScenarioDataCanEditProperties
              , ExerciseOnSubmitMsg       P.=. changeLinks newScenarioDataOnSubmitMessage
              ] -- optionally update image, geometry files:
              ++ ((ExerciseImage    P.=.) <$> maybeToList mImg)
              ++ ((ExerciseGeometry P.=.) <$> maybeToList mGeo)
            redirect $ ExerciseEditR exId
          Nothing -> do
            -- create
            invitationSecret <- liftIO generateInvitationSecret
            case (mImg, mGeo) of
              (Just img, Just geo) -> do
                runDB $ insert_ Exercise {
                    exerciseDescription       = newScenarioDataDescription
                  , exerciseImage             = img
                  , exerciseGeometry          = geo
                  , exerciseScale             = newScenarioDataScale
                  , exerciseCanAddDeleteGeom  = newScenarioDataCanAddDeleteGeom
                  , exerciseCanEditProperties = newScenarioDataCanEditProperties
                  , exerciseInvitationSecret  = invitationSecret
                  , exerciseOnSubmitMsg       = changeLinks newScenarioDataOnSubmitMessage
                  }
                showForm (Just dat) [] widget enctype
              _ -> showFormError ["Please upload both an image and geometry."]
                     widget enctype
  where
    --change links to use `onclick` so they work in reflex renderer:
    changeLinks = (*=~/ [edBS|(<a[^>]*)>///$1 onclick="window.open(this.href)" target="_blank">|])
                . (*=~/ [edBS|onclick=\"[^\"]*\"///|])
                . (*=~/ [edBS|target=\"[^\"]*\"///|])
                . toStrict . renderHtml
    getBs file = Just . LB.toStrict <$> (runResourceT $ fileSource file $$ CB.sinkLbs)
    showFormWidget = showForm Nothing []
    showFormError = showForm Nothing
    showForm ::
           Maybe ExerciseData
        -> [Text]
        -> WidgetT App IO ()
        -> Enctype
        -> HandlerT App IO Html
    showForm mr msgs widget enctype = do
        scenarioWidgets <- getScenarioCards
        adminLayout "Exercise editor" $ do
            setTitle "qua-kit - exercise editor"
            $(widgetFile "mooc/admin/exercise-editor")

generateInvitationSecret :: IO Text
generateInvitationSecret = T.pack <$> replicateM 16 (randomRIO ('a', 'z'))

data ExerciseData = ExerciseData
    { newScenarioDataDescription       :: Text
    , newScenarioDataImage             :: Maybe FileInfo
    , newScenarioDataScale             :: Double
    , newScenarioDataGeometry          :: Maybe FileInfo
    , newScenarioDataCanAddDeleteGeom  :: Bool
    , newScenarioDataCanEditProperties :: Bool
    , newScenarioDataOnSubmitMessage   :: Html
    }

-- | renders data from supplied exercise or default values
exerciseForm :: Maybe Exercise -> AForm Handler ExerciseData
exerciseForm mE = ExerciseData <$>
  areq textField (labeledField "description") (exerciseDescription <$> mE) <*>
  aopt fileField (labeledField "image") Nothing <*>
  areq doubleField (labeledField "scale (obsolete)")
    (Just $ fromMaybe 0.5 $ exerciseScale <$> mE) <*>
  aopt fileField (labeledField "geometry") Nothing <*>
  areq boolField (labeledField "Allow students to add/delete objects.")
    (Just $ fromMaybe False $ exerciseCanAddDeleteGeom <$> mE) <*>
  areq boolField (labeledField "Allow students to edit object properties.")
    (Just $ fromMaybe False $ exerciseCanEditProperties <$> mE) <*>
  areq htmlField (labeledField "On-submit html message. Use ${userId}, ${userName}, and ${exerciseId} to customize it.")
     (Just $ fromMaybe
        [shamlet|
           <h5>Thank you, ${userName}!
           <p>Your design submission has been saved.
              Though you can continue working on it and re-submit it later.
           <a href="https://httpbin.org/get?userId=${userId}&userName=${userName}&exId=${exerciseId}">
              Proceed with a personalized link
        |]
        $ preEscapedText . exerciseOnSubmitMsg <$> mE )

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

-- | render edit GUI
getExerciseEditR :: ExerciseId -> Handler Html
getExerciseEditR exerciseId = do
    requireAdmin
    exercise <- runDB $ get404 exerciseId
    let Exercise {..}  = exercise
    ((_, widget), enctype) <-
        runFormPost $ renderBootstrap3 BootstrapBasicForm $ exerciseForm $ Just exercise
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
             [ "Edit exercise"
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
