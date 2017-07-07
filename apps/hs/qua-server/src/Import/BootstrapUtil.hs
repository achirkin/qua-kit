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
