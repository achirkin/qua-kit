-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Mooc.Survey
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
--
--
-----------------------------------------------------------------------------

module Handler.Mooc.Survey
  ( getSurveyR, postSurveyR
  ) where


import Import
import Yesod.Form.Bootstrap3


postSurveyR :: Handler Html
postSurveyR = do
  ((formResult, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm testForm
  case formResult of
    FormSuccess answers -> do
      muserId <- maybeAuthId
      runDB . forM_ answers $ \(question, manswer) -> case manswer of
         Nothing -> return ()
         Just answer -> insert_ $ Survey muserId question answer
      fullLayout Nothing "Qua-kit user survey" $ do
        -- render all html
        [whamlet|
          <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9 col-xs-9">
              <div.card>
                    <div.card-main>
                      <div.card-header>
                        <div.card-inner.>
                          <h3.h5.margin-bottom-no.margin-top-no>
                            Thank you!
                      <div.card-inner>
                       Your answers have been saved.
        |]
    _ -> do
      fullLayout Nothing "Qua-kit user survey" $ do

        -- render all html
        [whamlet|
          <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9 col-xs-9">
              <div.card>
                    <div.card-main>
                      <div.card-header>
                        <div.card-inner.>
                          <h3.h5.margin-bottom-no.margin-top-no>
                            Dear edX student!
                      <div.card-inner>
                       <p>
                        Thank you for your participation in the course and in qua-kit series of exercises.
                        As you may know, qua-kit is an experimental educational and research platform.
                       <p>
                        You will help us a lot if you answer a series of questions related to your experience in qua-kit.
                        This should take not more than five minutes.
                        All questions are optional, so feel free to omit some of them if you do have difficulties with any.

              <div.card>
                    <div.card-main>
                      <div.card-inner>
                        <form role=form method=post action=@{SurveyR} enctype=#{formEnctype}>
                          ^{formWidget}
                          <button type="submit" .btn .btn-default>Submit
        |]

getSurveyR :: Handler Html
getSurveyR = postSurveyR


testForm :: AForm Handler [(Text,Maybe Text)]
testForm = sequenceA
         $ textInput "Your name"
         : boolInput "Were you able to run the design exercise in a browser?"
         : longTextInput "What kind of technical problems you did experience, if any?"
         : intInput "How many times did you submit your design?"
         : intInput "How many hours did you spend on your design in total?"
         : boolInput "Do you agree with your final design rating (comared to others)?"
         : boolInput "Do you feel the design rating is fair?"
         : longTextInput "If you think rating is not fair, why?"
         : boolInput "Did you change your design after any feedback from other students?"
         : boolInput "Did you look at other design submissions in the gallery?"
         : boolInput "Were the four design criteria useful for you? [Visibility, Centrality, Connectivity, Accessibility]"
         : boolInput "Were the design criteria descriptions useful and understandable?"
         : textInput "If you were asked to remove one criterion, which one would it be?"
         : textInput "If you were asked to add one more criterion, which one would it be?"
         : longTextInput "Could you comment on the criteria? Would you add some others or remove current ones?"
         : longTextInput "Please, add any other comments or suggestions about the qua-kit design exercise here:"
         : longTextInput "Please, add any other comments or suggestions about the qua-kit system here:"
         : boolInput "Did you search for an additional information about the case study (it was a small district in Cape Town)?"
         : longTextInput "What kind of contextual information did you use in your submission, if any?"
         : longTextInput "We would like to change a case study for the next run of the course. Would you suggest any place?\
              \ It must be a similar size and context existing settlement. Give a link to google map location and some reasoning, if you do not mind.\n\
              \ [If we select your proposed location, we will credit the name you specified above]."
         : longTextInput "Now, couple questsions regarding data collection exercise.\n\
                         \What could be changed / added to the website to improve data collection and/or its visualisation?\
                         \Please also explain why this would be an improvement."
         : longTextInput "Would it be helpful to allow uploading pictures? Please also explain why this would be an improvement."
         : []

textInput :: Text -> AForm Handler (Text,Maybe Text)
textInput n = (,) n <$> aopt textField (bfs n) Nothing


boolInput :: Text -> AForm Handler (Text,Maybe Text)
boolInput n = (,) n . fmap (pack . show) <$> aopt boolField (bfs n) Nothing

longTextInput :: Text -> AForm Handler (Text,Maybe Text)
longTextInput n = (,) n . fmap unTextarea <$> aopt textareaField (bfs n) Nothing


intInput :: Text -> AForm Handler (Text,Maybe Text)
intInput n = (,) n . fmap (pack . (show :: Int -> String)) <$> aopt intField (bfs n) Nothing
