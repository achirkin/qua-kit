-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin
-- Stability   :  experimental
--
-- An implementation of a simple luci service.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Luci.Connect
import Luci.Connect.Base
import Luci.Messages
import Data.Aeson
import Data.Aeson.Types
import Data.Conduit
import Data.Text
import Data.Monoid ((<>))


import qualified Data.Vector as V

-- * Define state machine
-- | First of all, let us define the state of our program
--   - all mutalble variables we want to keep

-- | Implement a state machine for the service.
--   We preserve the state in this data type throughout execution of the service.
data ProgramState = ProgramState
  { messagesProcessed :: Int
    -- ^ Here we will store number of messages received from server (Luci)
  , lastError         :: Maybe String
    -- ^ Here we will store last error, if there was any
  } deriving Show

-- | Define a default value for our program
initialState :: ProgramState
initialState = ProgramState 0 Nothing


-- | Entry point of a program
main :: IO ()
main = do
  -- use helper function that handles a lot of boilerplate for us
  lastState <- runReallySimpleLuciClient initialState $ do
    -- there is also logging embedded into the program.
    -- you can use logDebugN, logInfoN, logWarnN, logErrorN, logOtherN
    -- or anything else supported by MonadLogger
    logInfoN "Registering service"
    -- initial action
    registerService
    -- next action (looped using tail call)
    conduitActions
  -- program finished; print the state
  print lastState


-- | The first message to luci must be
registerService :: Conduit LuciMessage
                          (LuciProgram ProgramState)
                          (LuciProcessing Text LuciMessage)
registerService = yieldMessage $ simpleMessage RemoteRegister
    { exampleCall = object
              [ "run" .= ("TutorialService" :: Text)
              , "x" .= (4 :: Double)
              , "y" .= (2.5 :: Double)
              ]
    , serviceName = "TutorialService"
    , description = "not decided yet, what to do"
    , inputs = Just $ object [ "x" .= ("number" :: Text)
                             , "y" .= ("number" :: Text)
                             , "run" .= ("AddingNumbers" :: Text)
                             ]
    , outputs = Just $ object [ "XOR result" .= ("number" :: Text)
                              , "XOR error" .= ("string" :: Text)
                              ]
    }

-- | This is our main conuit program
conduitActions :: Conduit LuciMessage
                         (LuciProgram ProgramState)
                         (LuciProcessing Text LuciMessage)
conduitActions = do
  -- get current state of a program
  currentState <- get
  -- wait for a message from luci
  mmsg <- await
  case mmsg of
    -- nothing means END OF INPUT in Conduit world
    Nothing ->
      logWarnN "Received nothing, so connection is closed and program finishes"
    -- receive actual message
    Just msg -> do
      -- let's log some program state!
      logInfoN $ "Got message from luci. Counter: "
                  <> pack (show $ messagesProcessed currentState)
      -- do some stuff in the IO monad
      (newState', mresponseMsg) <- liftIO $ ioActions msg currentState
      newState <- case lastError newState' of
        Nothing -> return newState'
        Just er -> do
          logErrorN $ pack er
          return newState' { lastError = Nothing }
      -- save the prorgam state
      put newState
      -- send the response message back
      case mresponseMsg of
        Nothing   -> return ()
        Just rmsg -> yieldMessage rmsg
      -- recursively continue processing awaiting for a next message
      conduitActions

-- | It is not a nice approach in general, but makes it easier to undesrand.
--   This part of code is executed in IO monad -- no tricky types, no complications.
ioActions :: LuciMessage -> ProgramState -> IO (ProgramState, Maybe LuciMessage)
ioActions msg curState = do
  -- we can print something into console;
  -- though it is not recommended
  putStrLn "Printing hello from ioActions!"
  return $ case fromMessage msg of
     -- update program state with error
     Error errmsg   -> (curState { lastError = Just errmsg
                                 , messagesProcessed = messagesProcessed curState + 1
                                 }, Nothing)
     -- successfully parsed message; update state and give response
     Success (RunCalculate x y) ->
       ( curState { messagesProcessed = messagesProcessed curState + 1
                  }
       , Just . simpleMessage . object $
                    [ "result" .= object [ "sum" .= (x + y) ]]
       )

-- * Helper data types

-- | We define data type which will be used to parse JSON
data RunCalculate = RunCalculate Double Double

instance FromJSON RunCalculate where
  parseJSON (Object v) = RunCalculate
                   <$> v .: "x"
                   <*> v .: "y"
  parseJSON invalid = typeMismatch "RunCalculate" invalid
