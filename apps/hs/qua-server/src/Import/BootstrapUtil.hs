{-# OPTIONS_HADDOCK hide, prune #-}
module Import.BootstrapUtil where

import Import

bootstrapSelectFieldList :: (Eq a, RenderMessage site FormMessage)
                         => [(Text, a)] -> Field (HandlerT site IO) a
bootstrapSelectFieldList opts =
    Field {
      fieldParse   = parse
    , fieldView    = viewFunc
    , fieldEnctype = enc
    }
  where
    (Field parse view enc) = selectFieldList opts
    viewFunc id' name _attrs eitherText required = do
        let select = view id' name [("class", "form-control")] eitherText required
        [whamlet|
          <div .form-group>
            ^{select}
        |]

-- based on https://www.stackage.org/haddock/lts-8.23/yesod-form-1.4.12/src/Yesod.Form.Fields.html#dayField
bootstrapDayField :: Monad m => RenderMessage (HandlerSite m) FormMessage
                             => Field m Day
bootstrapDayField = Field
    { fieldParse = parseHelper $ parseDate . unpack
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
        <div .form-group>
          <input .form-control
            id="#{theId}"
            name="#{name}"
            *{attrs}
            type="date"
            :isReq:required=""
            value="#{showVal val}"
            >
      |]
    , fieldEnctype = UrlEncoded
    }
  where showVal = either id (pack . show)
