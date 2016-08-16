module Handler.Home where

import Import

import Handler.Home.LoadingSplash
import Handler.Home.PopupHelp
import Handler.Home.PopupSave
import Handler.Home.UIButtons
import Handler.Home.PanelServices
import Handler.Home.PanelGeometry
import Handler.Home.PanelInfo
import Handler.Home.LuciConnect

getHomeR :: Handler Html
getHomeR = renderQuaView

postHomeR :: Handler Html
postHomeR = renderQuaView


renderQuaView :: Handler Html
renderQuaView = do
  -- connecting form + conteiners for optional content
  (lcConnectedClass, lcDisconnectedClass, luciConnectForm) <- luciConnectPane
  (popupScenarioList, luciScenariosPane) <- luciScenarios
  defaultLayout $ do

    -- add qua-view dependencies
    toWidgetHead
      [hamlet|
        <script src="@{StaticR js_numeric_min_js}" type="text/javascript">
        <script src="@{StaticR js_qua_view_js}"    type="text/javascript">
      |]

    -- write a function to retrieve settings from qua-server to qua-view
    toWidgetHead
      [julius|
        window['getQuaViewSettings'] = function getQuaViewSettings(f){
          var qReq = new XMLHttpRequest();
          qReq.onload = function (e) {
            if (qReq.readyState === 4) {
              if (qReq.status === 200) {
                f(JSON.parse(qReq.responseText));
              } else {f({});}
            }
          };
          qReq.onerror = function (e) {f({});};
          qReq.open("GET", "@{QuaViewSettingsR}", true);
          qReq.send();
        };
      |]

    setTitle "qua-kit"

    -- render all html
    $(widgetFile "qua-view")
