
module Handler.Mooc.Admin.ReviewRequest
    ( getAdminReviewRequestR
    , postAdminReviewRequestR
    ) where

import Data.Time.Calendar (addDays, showGregorian)
import qualified Database.Persist.Sql as P
import Text.Shakespeare.Text (st)
import Database.Esqueleto
import Handler.Mooc.Admin
import Import hiding ((/=.), (==.), (=.), (<.), isNothing, on, update, groupBy, count)
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
    adminLayout "Request reviews" $ do
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
  let whereTask = maybe [] (\tId -> [CurrentScenarioTaskId P.==. tId]) (mtaskId params)
  scenarios <- runDB $ selectList (whereTask ++ [
                   CurrentScenarioGrade P.==. Nothing
                 , CurrentScenarioLastUpdate P.<. onlyBefore params
               ]) []
  statusTxt <-
    if length scenarios > 0 then do
      render <- getUrlRender
      let toLink (Entity _ sc) = render $ SubmissionViewerR
                                            (currentScenarioTaskId sc)
                                            (currentScenarioAuthorId sc)
      let scLinks = fmap toLink scenarios
          browseLink = case mtaskId params of
                          Nothing -> render BrowseProposalsForExpertsNR
                          Just i  -> render $ BrowseProposalsForExpertsR i
      case mexpertId params of
        Just expertId -> do
          mexpert <- runDB $ get expertId
          case mexpert of
            Just expert -> do
              sendReviewRequestMail (onlyBefore params) browseLink scLinks expert
              return "Email sent..."
            Nothing -> return "Expert not found"
        Nothing -> do
          experts <- selectExperts
          _ <- forM experts $ \(Entity _ ex) -> sendReviewRequestMail (onlyBefore params) browseLink scLinks ex
          return "Emails sent..."
    else
      return "No scenarios to review. Email not sent."
  setMessage statusTxt
  redirect AdminReviewRequestR

sendReviewRequestMail :: UTCTime -> Text -> [Text] -> User -> Handler ()
sendReviewRequestMail beforeT browseLink scLinks expert =
  case userEmail expert of
    Just email -> do
      let mailText = [st|Dear #{userName expert},

We would like to let you know that #{show $ length scLinks} oldest design submissions require grading in qua-kit.
Please, use the following link to see them (you don't need to grade designs submitted after #{showGregorian $ utctDay beforeT}):
  #{browseLink}

Note on grading:
  You can set a grade from 1 to 5 {stars}. The grade accounts for 40% of actual user grade at edX.
  This means 1 star is equivalent to 0.6 of edX grade and 5 stars is equivalent to 1.0 of edX grade.
  Therefore, setting 1 star is a normal practice; but only really outstanding solutions deserve 5 stars,
  because 5 stars correspond to 100% rating in qua-kit gallery.
  Also, in the expert review form, you have to write at least 80 characters of explanation for the grade you give.

Here is the full list of currently ungraded submissions:
#{unlines scLinks}

Thank you for your help!
                         |]
      $(logDebug) mailText
      liftIO $ Mail.renderSendMail $ Mail.simpleMail'
          (Mail.Address Nothing email) --to address
          (Mail.Address (Just "ETH qua-kit") "noreply@qua-kit.ethz.ch") --from
          "Please help review the following submissions" --subject
            $ fromStrict mailText
    Nothing -> return ()


selectExperts :: Handler [Entity User]
selectExperts = runDB $
  selectList [UserRole P.==. UR_EXPERT, UserEmail P./<-. [Nothing]] []

selectTasks :: Handler [TaskData]
selectTasks = fmap (fmap $ second unValue) . runDB $
  select $ from $ \(InnerJoin scenario scProblem) -> do
    on $ scenario ^. CurrentScenarioTaskId ==. scProblem ^. ScenarioProblemId
    where_ $ isNothing (scenario ^. CurrentScenarioGrade)
    groupBy $ scProblem ^. ScenarioProblemId
    return (scProblem, count $ scenario ^. CurrentScenarioId)
