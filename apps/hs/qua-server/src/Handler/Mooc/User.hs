{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.User
  ( maybeFetchExerciseId
  , postSetupLocalAccount
  , setupLocalAccountFromExistingUserW
  , getEnrollExerciseR
  , getEnrollNewUserExerciseR
  ) where


import Import
import Control.Monad.Trans.Except
import Yesod.Auth.Email (saltPass, registerR)
import Database.Persist.Sql (fromSqlKey)

-- | Try to fetch exercise id from the user session first,
--   then try to get the last enrolled exercise id.
maybeFetchExerciseId :: UserId -> Handler (Maybe ExerciseId)
maybeFetchExerciseId usrId = do
  mSessionExId <- getsSafeSession userSessionCurrentExerciseId
  case mSessionExId of
    Just exId -> return $ Just exId
    Nothing -> do
      muserExercise <- runDB $ selectFirst
          [UserExerciseUserId ==. usrId]
          [Desc UserExerciseEnrolled]
      return $ userExerciseExerciseId . entityVal <$> muserExercise

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


getEnrollExerciseR :: ExerciseId -> Text -> Handler Html
getEnrollExerciseR exId secret = do
    muserId <- maybeAuthId
    case muserId of
      Nothing -> redirect $ (AuthR registerR,  [("exercise", tshow $ fromSqlKey exId), ("invitation-secret", secret)])
      Just uId -> do
        ex <- runDB $ get404 exId
        unless (exerciseInvitationSecret ex == secret) notFound
        $(logDebug) $ "Enrolling new user in exercise " <> tshow (fromSqlKey exId)
        setSafeSession userSessionCurrentExerciseId exId
        time   <- liftIO getCurrentTime
        void $ runDB $ upsert (UserExercise uId exId time)
          [ UserExerciseUserId =. uId
          , UserExerciseExerciseId =. exId
          , UserExerciseEnrolled =. time]
        redirect $ RedirectToQuaViewEditR

getEnrollNewUserExerciseR :: ExerciseId -> Text -> Handler Html
getEnrollNewUserExerciseR exId secret = clearCreds False >> getEnrollExerciseR exId secret



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
                You can setup a full password-protected acount here to login #
                directly on our site. #
                To get a local account, choose your login e-mail and password.
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
