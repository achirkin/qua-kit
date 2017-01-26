{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- import           Control.Monad.IO.Class      (liftIO)
-- import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]
