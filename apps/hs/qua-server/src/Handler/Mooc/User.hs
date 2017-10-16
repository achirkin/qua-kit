{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.User
  ( maybeFetchExerciseId
  , postSetupLocalAccount
  , setupLocalAccountFromExistingUserW
  ) where


import Import
import Control.Monad.Trans.Except
import Yesod.Auth.Email (saltPass)

maybeFetchExerciseId :: UserId -> Handler (Maybe ScenarioProblemId)
maybeFetchExerciseId usrId = do
  muserExercise <- runDB $ getBy $ UserExerciseUnique usrId
  return $ userExerciseExerciseId <$> entityVal <$> muserExercise

postSetupLocalAccount :: UserId -> Handler Value
postSetupLocalAccount uId = fmap send . runExceptT $ do
    mu <- lift $ lookupPostParam "username"
    username <- case mu of
       Nothing -> throwE "Need a username."
       Just t -> do
          when (length t < 5) $
            throwE "Username must be at least 5 characters long"
          return t
    muname <- lift . runDB $ selectFirst [UserEmail ==. Just username] []
    when (isJust muname) $
        throwE "This login email is already used."
    mp <- lift $ lookupPostParam "password"
    p <- case mp of
       Nothing -> throwE "Need a password"
       Just t -> do
          when (length t < 6) $
            throwE "Password must be at least 6 characters long"
          return t
    saltedP <- liftIO $ saltPass p
    lift . runDB $ do
        update uId [
            UserName     =. takeWhile (/= '@') username
          , UserEmail    =. Just username
          , UserPassword =. Just saltedP
          , UserVerified =. True
          ]
        setMessage "Thanks! Your account login created!"
    return True
  where
    send (Left v) = object ["error" .= String v]
    send (Right v) = object ["success" .= v]

setupLocalAccountFromExistingUserW :: UserId -> Widget
setupLocalAccountFromExistingUserW userId = do
    toWidgetHead
      [julius|
        function tryCreateAccount() {
          $.post(
            { url: '@{SetupLocalAccount userId}'
            , data: $('#createAccForm').serialize()
            , success: function(result){
                  if (result.success) {
                    window.location.replace("@{MoocHomeR}");
                  } else {
                    $('#errormsg').text(result.error);
                  }
              }
            });
        }
      |]
    [whamlet|
      <div class="col-lg-4 col-sm-6">
        <div class="card">
            <div class="card-main">
              <div class="card-header">
                <div class="card-inner">
                  <h1 class="card-heading">Setup local account
              <div class="card-inner">
                <b> You can always come back to your last submitted design #
                by following the exercise link at edX. #
                However, you can setup a full password-protected acount here to login #
                directly on our site. #
                To get a local account, choose your login username and password.
                <div.text-red #errormsg>
                <p class="text-center">
                  <form class="form" action="@{SetupLocalAccount userId}" method="post" #createAccForm>
                    <div class="form-group form-group-label">
                      <div class="row">
                        <div class="col-md-10 col-md-push-1">
                          <label class="floating-label" for="username">E-Mail
                          <input class="form-control" id="username" name="username" type="email" required>
                    <div class="form-group form-group-label">
                      <div class="row">
                        <div class="col-md-10 col-md-push-1">
                          <label class="floating-label" for="password">Password
                          <input class="form-control" id="password" name="password" type="password" required>
                    <div class="form-group">
                      <div class="row">
                        <div class="col-md-10 col-md-push-1">
                          <a.btn.btn-block.btn-red.waves-attach.waves-light.waves-effect onclick="tryCreateAccount()">Create
    |]
