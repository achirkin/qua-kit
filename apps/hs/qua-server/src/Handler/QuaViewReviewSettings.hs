module Handler.QuaViewReviewSettings
    ( getQuaViewReviewSettingsR
    ) where


import Data.Aeson (encode)
import Import
import Types

import Database.Persist.Sql (toSqlKey)


getQuaViewReviewSettingsR :: Handler TypedContent
getQuaViewReviewSettingsR = do
    app <- getYesod
    req <- waiRequest
    let routeUrl route = let appr = getApprootText guessApproot app req
                         in  yesodRender app appr route []

    let scId = toSqlKey 3--TODO: fix this line
    let writeReviewUrl = routeUrl (WriteReviewR scId)

    return $ TypedContent typeJson $ toContent $ encode $ ReviewSettings {
        postReviewUrl    = writeReviewUrl
      }
