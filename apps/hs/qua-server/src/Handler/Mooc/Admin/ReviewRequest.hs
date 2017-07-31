
module Handler.Mooc.Admin.ReviewRequest
    ( getAdminReviewRequestR
    , postAdminReviewRequestR
    ) where

import Data.Time.Calendar (addDays)
import Database.Persist.Sql (rawSql, Single (..))
import Handler.Mooc.Admin
import Import
import Import.BootstrapUtil
import qualified Network.Mail.Mime as Mail

data SendReviewParams = SendReviewParams {
      mexpertId  :: Maybe (Key User)
    , mtaskId    :: Maybe ScenarioProblemId
    , onlyBefore :: UTCTime
    }

type TaskData = (Entity ScenarioProblem, Int)

getAdminReviewRequestR :: Handler Html
getAdminReviewRequestR = do
    requireAdmin
    experts <- selectExperts
    tasks   <- selectTasks
    defaultDay <- lift (getCurrentTime >>= return . addDays (-7) . utctDay)
    (reviewRequestFormWidget, _) <-
      generateFormPost $ reviewRequestForm experts tasks defaultDay
    fullLayout Nothing "Request reviews" $ do
        setTitle "qua-kit - request reviews"
        toWidgetHead $
          [cassius|
            .form-inline
              .form-group
                display: inline-block
                margin-right: 15px
          |]
        $(widgetFile "mooc/admin/review-request")

postAdminReviewRequestR :: Handler Html
postAdminReviewRequestR = do
    requireAdmin
    experts <- selectExperts
    tasks   <- selectTasks
    ((res, _), _) <- runFormPost $ reviewRequestForm experts tasks $ ModifiedJulianDay 0
    case res of
      (FormSuccess params) -> reviewRequest params
      _ -> defaultLayout [whamlet|<p>Invalid input</p>|]


reviewRequestForm :: [Entity User] -> [TaskData] -> Day -> Html
                  -> MForm Handler (FormResult SendReviewParams, Widget)
reviewRequestForm experts tasks defaultDay extra = do
  let expertsList = ("All experts", Nothing) :
        map (\(Entity usrId ex) -> (userName ex, Just usrId)) experts
  let toOption (Entity scpId scp, ugCount) =
        (scenarioProblemDescription scp ++
        " (" ++ (pack $ show ugCount) ++ " in need of grading)", Just scpId)
  let tasksList = ("All tasks", Nothing) : map toOption tasks
  (expertRes, expertView) <- mreq (bootstrapSelectFieldList expertsList) "" Nothing
  (taskRes, taskView) <- mreq (bootstrapSelectFieldList tasksList) "" Nothing
  (onlyBeforeRes, onlyBeforeView) <- mreq bootstrapDayField "" $ Just defaultDay
  let toTime day = UTCTime day $ fromInteger 0
  let params = SendReviewParams <$> expertRes
                                <*> taskRes
                                <*> fmap toTime onlyBeforeRes
  let widget = do
        [whamlet|
          #{extra}
          ^{fvInput expertView}
          ^{fvInput taskView}
          ^{fvInput onlyBeforeView}
          <input type=submit value="Send Mail" class="btn btn-default">
        |]
  return (params, widget)

reviewRequest :: SendReviewParams -> Handler Html
reviewRequest params = do
  let whereTask = maybe [] (\tId -> [CurrentScenarioTaskId ==. tId]) (mtaskId params)
  scenarios <- runDB $ selectList (whereTask ++ [
                   CurrentScenarioGrade ==. Nothing
                 , CurrentScenarioLastUpdate <. onlyBefore params
               ]) []
  statusTxt <-
    if length scenarios > 0 then do
      render <- getUrlRender
      let toLink (Entity _ sc) = render $ SubmissionViewerR
                                            (currentScenarioTaskId sc)
                                            (currentScenarioAuthorId sc)
      let scLinks = fmap toLink scenarios
      case mexpertId params of
        Just expertId -> do
          mexpert <- runDB $ get expertId
          case mexpert of
            Just expert -> do
              sendReviewRequestMail scLinks expert
              return "Email sent..."
            Nothing -> return "Expert not found"
        Nothing -> do
          experts <- selectExperts
          _ <- forM experts $ \(Entity _ ex) -> sendReviewRequestMail scLinks ex
          return "Emails sent..."
    else
      return "No scenarios to review. Email not sent."
  let statusTxt' = statusTxt::Text
  defaultLayout [whamlet|
                   <p>#{ statusTxt' }
                   <p><a onclick="window.history.back()" href="#">Back
                |]

sendReviewRequestMail :: [Text] -> User -> Handler ()
sendReviewRequestMail scLinks expert =
  case userEmail expert of
    Just email -> do
      let mailText = intercalate "\n\n" [
                         "Dear " <> userName expert
                       , "The following submissions are in need of reviewing:"
                       , intercalate "\n" scLinks
                       , "Thank you for your help!"
                       ]
      $(logDebug) mailText
      liftIO $ Mail.renderSendMail $ Mail.simpleMail'
          (Mail.Address Nothing email) --to address
          (Mail.Address (Just "ETH qua-kit") "noreply@qua-kit.ethz.ch") --from
          "Please help review the following submissions" --subject
            $ fromStrict mailText
    Nothing -> return ()

selectExperts :: Handler [Entity User]
selectExperts = runDB $
  selectList [UserRole ==. UR_EXPERT, UserEmail /<-. [Nothing]] []

selectTasks :: Handler [TaskData]
selectTasks = runDB $ (map getVal) <$> rawSql query []
  where
    getVal (scp, Single ungradedCount) = (scp, ungradedCount)
    query = unlines [
            "SELECT ??, ungraded_count"
          , "FROM scenario_problem"
          , "JOIN ( SELECT task_id, count(*) as ungraded_count"
          , "       FROM current_scenario"
          , "       WHERE grade IS NULL"
          , "       GROUP BY task_id"
          , "     ) s"
          , "ON scenario_problem.id = s.task_id"
          , ";"
          ]
