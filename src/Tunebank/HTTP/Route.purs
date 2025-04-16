module Tunebank.HTTP.Route
  ( Route(..)
  , route
  , router
  )
  where

import Prelude hiding ((/))

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (stringify)
import Data.Array (intercalate)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import HTTPurple (Request, Method(..), notFound, ok, ok')
import HTTPurple.Body (RequestBody)
import HTTPurple.Body (toString) as Body
import HTTPurple.Headers (RequestHeaders)
import HTTPurple.Json (jsonHeaders)
import HTTPurple.Response (Response)
import HTTPurple.Routes (catchAll)
import Routing.Duplex (RouteDuplex', optional, root, segment, string, int)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Tunebank.Database.Comment (getComments, getComment)
import Tunebank.Database.Genre (getGenres)
import Tunebank.Database.Rhythm (getRhythmsForGenre)
import Tunebank.Database.Search (SearchParams, buildSearchExpression)
import Tunebank.Database.Tune (getTuneAbc, getTuneRefs, deleteTune, upsertTune)
import Tunebank.Database.User (getUserRecords, getUserRecord)
import Tunebank.Environment (Env)
import Tunebank.HTTP.Authentication (getAuthorization, withAdminAuthorization, withAnyAuthorization)
import Tunebank.HTTP.Headers (abcHeaders, preflightAllOrigins)
import Tunebank.HTTP.Response (customForbidden, customBadRequest)
import Tunebank.Logic.AbcMetadata (buildMetadata)
import Tunebank.Logic.Codecs (encodeComments, encodeComment, encodeGenres, encodeRhythms, encodeTuneRefs, encodeUserRecords, encodeUserRecord)
import Tunebank.Types (Authorization, Comment, Genre, Rhythm, UserName, TuneRef)
import Yoga.Postgres (Pool, withClient)

data Route
  = Home
  | Genres
  | Rhythms Genre
  | Tunes Genre
  | Tune Genre String
  | Search Genre SearchParams
  | Comments Genre String 
  | Comment Int
  | Users
  | User UserName
  | CheckRequest
  | CatchAll (Array String)


derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

-- | a string matching segment for the Genre newtype
genreSeg :: RouteDuplex' Genre
genreSeg = _Newtype segment

-- | a string matching segment for the UserName newtype
userSeg :: RouteDuplex' UserName
userSeg = _Newtype segment

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "Genres": "genre" / noArgs
  , "Rhythms": "genre" / genreSeg
  , "Tunes": "genre" / genreSeg / "tune" 
  , "Tune": "genre" / genreSeg / "tune" / (string segment) 
  , "Search":  "genre" / genreSeg / "search" ? 
       { title : optional <<< string
       , key : optional <<< string
       , rhythm : optional <<< string
       , source : optional <<< string
       , composer : optional <<< string
       , origin : optional <<< string
       , transcriber : optional <<< string
       , page : optional <<< int
       , sort : optional <<< string
       }
  , "Users": "user" / noArgs
  , "User": "user" / userSeg
  , "Comments": "genre" / genreSeg / "tune" / (string segment) / "comment"
  , "Comment": "comment" / (int segment)
  , "CheckRequest": "check" / noArgs
  , "CatchAll": catchAll
  }


router :: forall m. MonadAff m => MonadAsk Env m => Request Route -> m Response
router { route: Home } = homeRoute
router { route: Genres } = genreRoute
router { route: Rhythms genre } = rhythmRoute genre
router { route: Tunes genre, method : Options } = preflightOptionsRoute
router { route: Tunes genre, method : Post, headers, body } = upsertTuneRoute genre headers body
router { route: Tunes genre } = tunesRoute genre
router { route: Tune genre title, method: Delete, headers } = deleteTuneRoute genre title headers
router { route: Tune genre title } = tuneRoute genre title
router { route: Search genre params } = searchRoute genre params 
router { route: Comments genre title } = commentsRoute genre title
router { route: Comment id} = commentRoute id
router { route: Users, headers } = usersRoute headers
router { route: User user, headers } = userRoute user headers
router { route: CheckRequest, headers } = routeCheckRequest headers
router { route: CatchAll paths } = routeError paths

homeRoute :: forall m. MonadAff m => MonadAsk Env m => m Response
homeRoute = do 
  myName <- asks _.name
  ok ("tunebank 0.0.1 " <> myName)


genreRoute :: forall m. MonadAff m => MonadAsk Env m => m Response
genreRoute = do 
  dbpool :: Pool  <- asks _.dbpool
  genres :: Array Genre <- liftAff $ withClient dbpool $ do
    getGenres
  let
    json = stringify $ encodeGenres genres
  ok' jsonHeaders json


rhythmRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> m Response
rhythmRoute genre = do 
  dbpool :: Pool  <- asks _.dbpool
  rhythms :: Array Rhythm <- liftAff $ withClient dbpool $ do
    getRhythmsForGenre genre
  let
    json = stringify $ encodeRhythms rhythms
  ok' jsonHeaders json


tunesRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> m Response
tunesRoute genre = do 
  dbpool :: Pool  <- asks _.dbpool
  tunes :: Array TuneRef <- liftAff $ withClient dbpool $ do
    getTuneRefs genre []
  let
    json = stringify $ encodeTuneRefs tunes
  ok' jsonHeaders json


searchRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> SearchParams -> m Response
searchRoute genre params = do 
  let 
    searchExpression = buildSearchExpression params
  _ <- liftEffect $ logShow searchExpression
  dbpool :: Pool  <- asks _.dbpool
  tunes :: Array TuneRef <- liftAff $ withClient dbpool $ do
    getTuneRefs genre searchExpression
  let
    json = stringify $ encodeTuneRefs tunes
  ok' jsonHeaders json

tuneRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> String -> m Response
tuneRoute genre title = do 
  dbpool :: Pool  <- asks _.dbpool
  mTune <- liftAff $ withClient dbpool $ do
    getTuneAbc genre title
  maybe notFound (ok' abcHeaders) mTune

deleteTuneRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> String -> RequestHeaders -> m Response
deleteTuneRoute genre title headers = do 
  dbpool :: Pool  <- asks _.dbpool 
  liftAff $ withClient dbpool $ \c -> do
    eAuth ::  Either String Authorization <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      eResult <- deleteTune genre title auth.user c
      either customForbidden (const $ ok "") eResult


upsertTuneRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> RequestHeaders -> RequestBody -> m Response
upsertTuneRoute genre headers body = do 
  dbpool :: Pool  <- asks _.dbpool 
  liftAff $ withClient dbpool $ \c -> do
    eAuth ::  Either String Authorization <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      abc <- Body.toString body
      case (buildMetadata abc) of
        Left error -> 
          customBadRequest error
        Right validatedAbc -> do
          eResult <- upsertTune genre auth validatedAbc c
          either customForbidden (const $ ok "") eResult
          
commentsRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> String -> m Response
commentsRoute genre title = do 
  dbpool :: Pool  <- asks _.dbpool
  eComments :: (Either String (Array Comment)) <- liftAff $ withClient dbpool $ do
    getComments genre title
  case eComments of 
    Left error -> 
      customBadRequest error
    Right comments -> do   
      let
        json = stringify $ encodeComments comments
      ok' jsonHeaders json

commentRoute :: forall m. MonadAff m => MonadAsk Env m => Int -> m Response
commentRoute commentId = do 
  dbpool :: Pool  <- asks _.dbpool
  eComment :: (Either String Comment) <- liftAff $ withClient dbpool $ do
    getComment commentId
  case eComment of 
    Left error -> 
      customBadRequest error
    Right comment -> do   
      let
        json = stringify $ encodeComment comment
      ok' jsonHeaders json


usersRoute :: forall m. MonadAff m => MonadAsk Env m => RequestHeaders -> m Response
usersRoute headers = do 
  dbpool :: Pool  <- asks _.dbpool
  liftAff $ withClient dbpool $ \c -> do
    eAuth <- getAuthorization headers c
    withAdminAuthorization eAuth $ \_auth -> do
      users <- getUserRecords c
      let
        json = stringify $ encodeUserRecords users
      ok' jsonHeaders json

userRoute :: forall m. MonadAff m => MonadAsk Env m => UserName -> RequestHeaders -> m Response
userRoute user headers = do 
  dbpool :: Pool  <- asks _.dbpool
  mUser <- liftAff $ withClient dbpool $ do
    getUserRecord user
  let
    mJson = map (stringify <<< encodeUserRecord ) mUser
  maybe notFound (ok' jsonHeaders) mJson

preflightOptionsRoute :: forall m. MonadAff m => MonadAsk Env m => m Response
preflightOptionsRoute = do 
  ok' preflightAllOrigins ""

routeError :: forall m. MonadAff m => MonadAsk Env m => Array String -> m Response
routeError paths = do 
  let 
    fullPath = intercalate "/" paths
  ok ("we got a routing error: " <> fullPath )

routeCheckRequest :: forall m. MonadAff m => MonadAsk Env m => RequestHeaders -> m Response
routeCheckRequest headers = do 
  ok ("request headers: " <> show headers )


