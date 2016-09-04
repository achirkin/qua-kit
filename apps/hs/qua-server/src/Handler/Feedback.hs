-----------------------------------------------------------------------------
-- |
-- Module      :  Handler.Feedback
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Handler.Feedback
  ( getFeedbackR
  , postSubmitFeedbackR
  ) where


import Import

getFeedbackR :: Handler Html
getFeedbackR = fullLayout Nothing "Feedback" $ do
    setTitle "Feedback"
    submitForm <- newIdent
    textAreaDesc <- newIdent
    toWidgetBody $
        [hamlet|
          <div class="row">
            <div class="col-lg-9 col-md-9 col-sm-9 col-xs-9">

              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner.>
                      <h3.h5.margin-bottom-no.margin-top-no>
                        Submit an issue
                  <div.card-inner>
                    The best way to help us in developing this platform is to #
                    submit an issue on github. #
                    <a href="https://github.com/achirkin/qua-kit/issues">
                      https://github.com/achirkin/qua-kit/issues

              <div.card>
                <div.card-main>
                  <div.card-header>
                    <div.card-inner.>
                      <h3.h5.margin-bottom-no.margin-top-no>
                        Send us a message
                  <div.card-inner>
                    You can also send us a message using the form below.
                    <form ##{submitForm} method="post" action="@{SubmitFeedbackR}">
                      <div class="form-group form-group-label">
                         <label.floating-label for="#{textAreaDesc}">Write a suggestion or an issue report
                         <textarea.form-control.textarea-autosize form="#{submitForm}" id="#{textAreaDesc}" rows="1" name="description">
                      <div.card-action>
                        <p class="text-right">
                          <a.btn.btn-flat.btn-brand-accent.waves-attach.waves-effect onclick="$('##{submitForm}').submit();">
                            Send
        |]

postSubmitFeedbackR :: Handler Html
postSubmitFeedbackR = do
    userId <- maybeAuthId
    text <- fromMaybe "" <$> lookupPostParam "description"
    case text of
      "" -> redirect FeedbackR
      s -> do
        runDB . insert_ $ Feedback userId s
        setMessage "Thanks, your message has been sent!"
        redirect FeedbackR


