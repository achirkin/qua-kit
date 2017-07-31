
module Handler.Mooc.Admin.ReviewRequest
    ( getAdminReviewRequestR
    , postAdminReviewRequestR
    ) where

import Data.Time.Calendar (addDays)
import Handler.Mooc.Admin
import Import
import Import.BootstrapUtil
import qualified Network.Mail.Mime as Mail

data SendReviewParams = SendReviewParams {
      mexpertId  :: Maybe (Key User)
    , onlyBefore :: UTCTime
    }

getAdminReviewRequestR :: Handler Html
getAdminReviewRequestR = do
    requireAdmin
    experts <- selectExperts
    defaultDay <- lift (getCurrentTime >>= return . addDays (-7) . utctDay)
    (reviewRequestFormWidget, _) <-
      generateFormPost $ reviewRequestForm experts defaultDay
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
    ((res, _), _) <- runFormPost $ reviewRequestForm experts $ ModifiedJulianDay 0
    case res of
      (FormSuccess params) -> reviewRequest params
      _ -> defaultLayout [whamlet|<p>Invalid input</p>|]


reviewRequestForm :: [Entity User] -> Day -> Html
                  -> MForm Handler (FormResult SendReviewParams, Widget)
reviewRequestForm experts defaultDay extra = do
  let expertsList = ("All experts", Nothing) :
        map (\(Entity usrId ex) -> (userName ex, Just usrId)) experts
  (expertRes, expertView) <- mreq (bootstrapSelectFieldList expertsList) "" Nothing
  (onlyBeforeRes, onlyBeforeView) <- mreq bootstrapDayField "" $ Just defaultDay
  let toTime day = UTCTime day $ fromInteger 0
  let params = SendReviewParams <$> expertRes
                                <*> fmap toTime onlyBeforeRes
  let widget = do
        [whamlet|
          #{extra}
          ^{fvInput expertView}
          ^{fvInput onlyBeforeView}
          <input type=submit value="Send Mail" class="btn btn-default">
        |]
  return (params, widget)

reviewRequest :: SendReviewParams -> Handler Html
reviewRequest params = do
  scenarios <- runDB $ selectList [
                   CurrentScenarioGrade ==. Nothing
                 , CurrentScenarioLastUpdate <. onlyBefore params
               ] []
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
