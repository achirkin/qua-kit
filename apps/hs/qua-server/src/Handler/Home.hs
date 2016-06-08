module Handler.Home where

import Import

--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                              withSmallInput)
--import Text.Julius (RawJS (..))


getHomeR :: Handler Html
getHomeR = renderQuaView

postHomeR :: Handler Html
postHomeR = renderQuaView


renderQuaView :: Handler Html
renderQuaView = defaultLayout $ do
  toWidgetHead $
    [hamlet|
      <script src="@{StaticR js_LuciClient_js}" type="text/javascript">
      <script src="@{StaticR js_numeric_min_js}" type="text/javascript">
      <script src="@{StaticR js_misc_js}" type="text/javascript">
    |]
  setTitle "qua-kit"
  $(widgetFile "qua-view")
