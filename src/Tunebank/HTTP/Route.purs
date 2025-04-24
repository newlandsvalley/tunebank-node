module Tunebank.HTTP.Route
  ( Route(..)
  , route
  , router
  )
  where

import Prelude hiding ((/))

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (printJsonDecodeError, stringify)
import Data.Array (intercalate)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (maybe)
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
import Tunebank.Database.Comment (deleteComment, deleteComments, getComments, getComment, insertComment, updateComment)
import Tunebank.Database.Genre (getGenres)
import Tunebank.Database.Rhythm (getRhythmsForGenre)
import Tunebank.Database.Search (SearchParams, buildSearchExpression, defaultSearchParams)
import Tunebank.Database.Tune (getTuneAbc, deleteTune, upsertTune)
import Tunebank.Database.User (getUserRecord, insertUnregisteredUser, validateUser)
import Tunebank.Environment (Env)
import Tunebank.HTTP.Authentication (getAuthorization, withAdminAuthorization, withAnyAuthorization)
import Tunebank.HTTP.Headers (abcHeaders, preflightAllOrigins)
import Tunebank.HTTP.Response (customForbidden, customBadRequest)
import Tunebank.Logic.Api (getTuneRefsPage, getUserRecordsPage)
import Tunebank.Logic.AbcMetadata (buildMetadata)
import Tunebank.Logic.Codecs (decodeNewUser, decodeNewComment, encodeComments, encodeComment, encodeGenres, encodeRhythms,
        encodeTunesPage, encodeUserRecordsPage, encodeUserRecord)
import Tunebank.Pagination (TuneRefsPage, PagingParams, buildPaginationExpression, buildSearchPaginationExpression, defaultUserPagingParams)
import Tunebank.Types (Authorization, Comment, Genre, Rhythm, UserName)
import Tunebank.Config (PagingConfig)
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
  | UserSearch PagingParams
  | UserCheck
  | UserValidate String
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
  , "Rhythms": "genre" / genreSeg / "rhythm"
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
  , "UserSearch": "search" ? 
       { page : optional <<< int
       , sort : optional <<< string
       }
  , "UserCheck": "user" / "check" / noArgs
  , "UserValidate": "user" / "validate" / (string segment)
  , "Comments": "genre" / genreSeg / "tune" / (string segment) / "comment"
  , "Comment": "comment" / (int segment)
  , "CheckRequest": "check" / noArgs
  , "CatchAll": catchAll
  }


router :: forall m. MonadAff m => MonadAsk Env m => Request Route -> m Response
router { route: Home } = homeRoute
router { route: Genres } = genreRoute
router { route: Rhythms genre } = rhythmRoute genre
router { route: Tunes _genre, method : Options } = preflightOptionsRoute
router { route: Tunes genre, method : Post, headers, body } = upsertTuneRoute genre headers body
router { route: Tunes genre } = tunesRoute genre
router { route: Tune genre title, method: Delete, headers } = deleteTuneRoute genre title headers
router { route: Tune genre title } = tuneRoute genre title
router { route: Search genre params } = searchRoute genre params 
router { route: Comments _genre _title, method: Options } = preflightOptionsRoute
router { route: Comments genre title, method: Post, headers, body } = addCommentRoute genre title headers body
router { route: Comments genre title, method: Delete, headers } = deleteTuneCommentsRoute genre title headers
router { route: Comments genre title } = commentsRoute genre title
router { route: Comment _id, method: Options } = preflightOptionsRoute
router { route: Comment id, method: Delete, headers } = deleteCommentRoute id headers
router { route: Comment id, method: Post, headers, body } = updateCommentRoute id body headers 
router { route: Comment id} = commentRoute id
router { route: Users, method: Post, body} = insertUserRoute body
router { route: Users, headers } = usersRoute defaultUserPagingParams headers
router { route: User user, headers } = userRoute user headers
router { route: UserCheck, headers } = checkUserRoute headers
router { route: UserSearch params, headers } = usersRoute params headers
router { route: UserValidate uuid } = validateUserRoute uuid
router { route: CheckRequest, headers } = routeCheckRequest headers
router { route: CatchAll paths } = routeError paths

homeRoute :: forall m. MonadAff m => MonadAsk Env m => m Response
homeRoute = do 
  ok ("tunebank 0.0.1")


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
tunesRoute genre = 
  searchRoute genre defaultSearchParams


{-}
  dbpool :: Pool  <- asks _.dbpool
  tunes :: Array TuneRef <- liftAff $ withClient dbpool $ do
    getTuneRefs genre [] defaultPaginationExpression
  let
    json = stringify $ encodeTuneRefs tunes
  ok' jsonHeaders json
-}


searchRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> SearchParams -> m Response
searchRoute genre params = do 
  paging :: PagingConfig  <- asks _.paging
  let 
    searchExpression = buildSearchExpression params
    paginationExpression = buildSearchPaginationExpression params paging.defaultSize
  _ <- liftEffect $ logShow searchExpression
  dbpool :: Pool  <- asks _.dbpool
  tunesPage :: TuneRefsPage <- liftAff $ withClient dbpool $ do
    getTuneRefsPage genre searchExpression paginationExpression paging.defaultSize
  let
    json = stringify $ encodeTunesPage tunesPage 
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

addCommentRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> String -> RequestHeaders -> RequestBody -> m Response
addCommentRoute genre title headers body = do 
  dbpool :: Pool  <- asks _.dbpool 
  jsonString <- Body.toString body
  {- eNewComment :: Either JsonDecodeError NewComment -}
  case (decodeNewComment jsonString) of
    Left err -> 
      customBadRequest $ printJsonDecodeError err
    Right newComment -> do 
      liftAff $ withClient dbpool $ \c -> do
        eAuth ::  Either String Authorization <- getAuthorization headers c
        withAnyAuthorization eAuth $ \auth -> do
          eResult :: Either String Int <- insertComment genre title newComment auth.user c
          either customForbidden (show >>> ok) eResult
          
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

deleteCommentRoute :: forall m. MonadAff m => MonadAsk Env m => Int -> RequestHeaders -> m Response
deleteCommentRoute commentId headers = do 
  dbpool :: Pool  <- asks _.dbpool 
  liftAff $ withClient dbpool $ \c -> do
    eAuth ::  Either String Authorization <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      eResult <- deleteComment commentId auth c
      either customForbidden (const $ ok "") eResult

deleteTuneCommentsRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> String -> RequestHeaders -> m Response
deleteTuneCommentsRoute genre title headers = do 
  dbpool :: Pool  <- asks _.dbpool 
  liftAff $ withClient dbpool $ \c -> do
    eAuth ::  Either String Authorization <- getAuthorization headers c
    withAdminAuthorization eAuth $ \_auth -> do
      _result <- deleteComments genre title c
      ok ""

updateCommentRoute :: forall m. MonadAff m => MonadAsk Env m => Int -> RequestBody -> RequestHeaders -> m Response
updateCommentRoute id body headers = do 
  _ <- liftEffect $ logShow ("update comment " <> show id)
  jsonString <- Body.toString body
  {- eNewComment :: Either JsonDecodeError NewComment -}
  case (decodeNewComment jsonString) of
    Left err -> 
      customBadRequest $ printJsonDecodeError err
    Right updatedComment -> do 
      dbpool :: Pool  <- asks _.dbpool 
      liftAff $ withClient dbpool $ \c -> do
        eAuth ::  Either String Authorization <- getAuthorization headers c
        withAnyAuthorization eAuth $ \auth -> do
          eResult <- updateComment id updatedComment auth c
          either customForbidden (const $ ok "") eResult

usersRoute :: forall m. MonadAff m => MonadAsk Env m => PagingParams -> RequestHeaders -> m Response
usersRoute pagingParams headers = do 
  paging :: PagingConfig  <- asks _.paging
  let 
    paginationExpression = buildPaginationExpression pagingParams paging.defaultSize
  dbpool :: Pool  <- asks _.dbpool
  liftAff $ withClient dbpool $ \c -> do
    eAuth <- getAuthorization headers c
    withAdminAuthorization eAuth $ \_auth -> do
      usersPage <- getUserRecordsPage paginationExpression paging.defaultSize c
      let
        json = stringify $ encodeUserRecordsPage usersPage
      ok' jsonHeaders json

checkUserRoute :: forall m. MonadAff m => MonadAsk Env m => RequestHeaders -> m Response
checkUserRoute headers = do 
  dbpool :: Pool  <- asks _.dbpool
  liftAff $ withClient dbpool $ \c -> do
    eAuth <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      ok ("user " <> show auth.user <> " found")

userRoute :: forall m. MonadAff m => MonadAsk Env m => UserName -> RequestHeaders -> m Response
userRoute user headers = do 
  dbpool :: Pool  <- asks _.dbpool
  mUser <- liftAff $ withClient dbpool $ do
    getUserRecord user
  let
    mJson = map (stringify <<< encodeUserRecord ) mUser
  maybe notFound (ok' jsonHeaders) mJson

insertUserRoute :: forall m. MonadAff m => MonadAsk Env m => RequestBody -> m Response
insertUserRoute body = do 
  jsonString <- Body.toString body
  {- eNewUser :: Either JsonDecodeError NewUser -}
  case (decodeNewUser jsonString) of
    Left err -> 
      customBadRequest $ printJsonDecodeError err
    Right newUser -> do 
      dbpool :: Pool  <- asks _.dbpool
      eResult <- liftAff $ withClient dbpool $ do
        insertUnregisteredUser newUser
      either customBadRequest ok eResult


validateUserRoute :: forall m. MonadAff m => MonadAsk Env m => String -> m Response
validateUserRoute uuid = do 
  dbpool :: Pool  <- asks _.dbpool
  _ <- liftAff $ withClient dbpool $ do
    validateUser uuid 
  ok "validated"

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


