module Test.Utils 
  ( getTestCommentId 
  , withDBConnection) 
  where

import Prelude
import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Yoga.Postgres (Query(Query), Client, queryValue_, mkPool, withClient)
import Tunebank.Database.Utils (maybeIntResult)
import Tunebank.Environment (connectionInfo)

-- | get the id of the test comment set up by the installation script
-- | this is necessary because the comment id is not static - it changes with every test run
getTestCommentId :: Client -> Aff (Maybe Int)
getTestCommentId c = do
  let 
    query = "select id from comments where comment = 'This is a horrible tune' " 
  mId <- queryValue_ maybeIntResult (Query query :: Query (Maybe Int)) c
  pure $ join mId

-- | query the test database
withDBConnection :: forall a. (Client -> Aff a) -> Aff a
withDBConnection execution = do
  pool <- liftEffect $ mkPool connectionInfo
  withClient pool execution