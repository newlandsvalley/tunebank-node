module Tunebank.HTTP.Route
  ( Route(..)
  , route
  , router
  ) where

import Prelude hiding ((/))

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Argonaut (printJsonDecodeError, stringify)
import Data.Array (intercalate)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import HTTPurple (Request, Method(..), internalServerError, notFound, ok, ok')
import HTTPurple.Body (RequestBody)
import HTTPurple.Body (toString) as Body
import HTTPurple.Headers (RequestHeaders)
import HTTPurple.Json (jsonHeaders)
import HTTPurple.Lookup (lookup)
import HTTPurple.Response (Response, unauthorized)
import HTTPurple.Routes (catchAll)
import Routing.Duplex (RouteDuplex', optional, root, segment, string, int)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Tunebank.Config (PagingConfig)
import Tunebank.Database.Comment (deleteComment, deleteComments, getComments, getComment, insertComment, updateComment)
import Tunebank.Database.Genre (getGenres)
import Tunebank.Database.Rhythm (getRhythmsForGenre)
import Tunebank.Database.Search (SearchParams, buildSearchExpression, defaultSearchParams)
import Tunebank.Database.Tune (getTuneAbc, getTuneMetadata, deleteTune, upsertTune)
import Tunebank.Database.User (UserValidity(..), changeUserPassword, deleteUser, getUserName, getUserRecord, insertUser, validateUser)
import Tunebank.Environment (Env)
import Tunebank.HTTP.Authentication (getAuthorization, withAdminAuthorization, withAnyAuthorization, validateCorsOrigin)
import Tunebank.HTTP.Headers (abcHeaders, corsHeadersOrigin, corsHeadersAllOrigins, midiHeaders, preflightOrigin)
import Tunebank.HTTP.Response (customBadRequest, customErrorResponse)
import Tunebank.Logic.AbcMetadata (buildMetadata)
import Tunebank.Logging.Winston (logError, logInfo)
import Tunebank.Logic.Api (getTuneMidi, getTuneRefsPage, getUserRecordsPage)
import Tunebank.Logic.Codecs (decodeNewUser, decodeNewComment, decodeUserPassword, decodeUserPasswordOTP, encodeComments, encodeComment, encodeGenres, encodeRhythms, encodeTuneMetadata, encodeTunesPage, encodeUserRecordsPage, encodeUserRecord)
import Tunebank.Logic.Naming (safeFileName)
import Tunebank.Pagination (TuneRefsPage, PagingParams, buildPaginationExpression, buildSearchPaginationExpression)
import Tunebank.Tools.Mail (sendRegistrationMail, sendNewPasswordOTPMail, sendUserNameMail)
import Tunebank.Types (Authorization, Genre, Rhythm, Title, TuneMetadata, UserName(..))
import Yoga.Postgres (Pool, withClient)

data Route
  = Home
  | Genres
  | Rhythms Genre
  | Tunes Genre
  | Tune Genre Title
  | TuneAbc Genre Title
  | TuneMidi Genre Title
  | Search Genre SearchParams
  | Comments Genre Title
  | Comment Int
  | UserCheck
  | UserNewPasswordOTP -- One-Time-Password for a change password request
  | UserNewPassword -- The actual change password
  | UserGetName -- Get the user name from the password
  | Users PagingParams
  | User UserName
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

-- | a string matching segment for the Title newtype
titleSeg :: RouteDuplex' Title
titleSeg = _Newtype segment

-- | a string matching segment for the UserName newtype
userSeg :: RouteDuplex' UserName
userSeg = _Newtype segment

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "Genres": "genre" / noArgs
  , "Rhythms": "genre" / genreSeg / "rhythm"
  , "Tunes": "genre" / genreSeg / "tune"
  , "Tune": "genre" / genreSeg / "tune" / titleSeg
  , "TuneAbc": "genre" / genreSeg / "tune" / titleSeg / "abc"
  , "TuneMidi": "genre" / genreSeg / "tune" / titleSeg / "midi"
  , "Search": "genre" / genreSeg / "search" ?
      { title: optional <<< string
      , key: optional <<< string
      , rhythm: optional <<< string
      , source: optional <<< string
      , composer: optional <<< string
      , origin: optional <<< string
      , transcriber: optional <<< string
      , submitter: optional <<< string
      , page: optional <<< int
      , sort: optional <<< string
      }
  , "UserCheck": "user" / "check" / noArgs -- comes first otherwise 'check' taken as user name
  , "UserNewPassword": "user" / "newPassword" / noArgs -- ditto
  , "UserNewPasswordOTP": "user" / "newPasswordOTP" / noArgs -- ditto
  , "UserGetName": "user" / "getName" / noArgs -- ditto
  , "Users": "user" ?
      { page: optional <<< int
      , sort: optional <<< string
      }
  , "User": "user" / userSeg
  , "UserValidate": "user" / "validate" / (string segment)
  , "Comments": "genre" / genreSeg / "tune" / titleSeg / "comments"
  , "Comment": "comment" / (int segment)
  , "CheckRequest": "check" / noArgs
  , "CatchAll": catchAll
  }

router :: forall m. MonadAff m => MonadAsk Env m => Request Route -> m Response
router { route: Home } = homeRoute
router { route: Genres } = genreRoute
router { route: Rhythms genre } = rhythmRoute genre
router { route: Tunes _genre, method: Options, headers } = preflightOptionsRoute headers
router { route: Tunes genre, method: Post, headers, body } = upsertTuneRoute genre headers body
router { route: Tunes genre } = tunesRoute genre
router { route: Tune _genre _title, method: Options, headers } = preflightOptionsRoute headers
router { route: Tune genre title, method: Delete, headers } = deleteTuneRoute genre title headers
router { route: Tune genre title } = tuneRoute genre title
router { route: TuneAbc genre title } = tuneAbcRoute genre title
router { route: TuneMidi genre title } = tuneMidiRoute genre title
router { route: Search genre params } = searchRoute genre params
router { route: Comments _genre _title, method: Options, headers } = preflightOptionsRoute headers
router { route: Comments genre title, method: Post, headers, body } = addCommentRoute genre title headers body
router { route: Comments genre title, method: Delete, headers } = deleteTuneCommentsRoute genre title headers
router { route: Comments genre title } = commentsRoute genre title
router { route: Comment _id, method: Options, headers } = preflightOptionsRoute headers
router { route: Comment id, method: Delete, headers } = deleteCommentRoute id headers
router { route: Comment id, method: Post, headers, body } = updateCommentRoute id body headers
router { route: Comment id } = commentRoute id
router { route: UserCheck, method: Options, headers } = preflightOptionsRoute headers
router { route: UserCheck, headers } = checkUserRoute headers
router { route: UserNewPassword, method: Options, headers } = preflightOptionsRoute headers
router { route: UserNewPassword, body } = userNewPasswordRoute body
router { route: UserNewPasswordOTP, method: Options, headers } = preflightOptionsRoute headers
router { route: UserNewPasswordOTP, body } = userNewPasswordOTPRoute body
router { route: UserGetName, method: Options, headers } = preflightOptionsRoute headers
router { route: UserGetName, body } = userGetNameRoute body
router { route: Users _params, method: Options, headers } = preflightOptionsRoute headers
router { route: Users _params, method: Post, body } = insertUserRoute body
router { route: Users params, headers } = usersRoute params headers
router { route: User _user, method: Options, headers } = preflightOptionsRoute headers
router { route: User user, method: Delete, headers } = deleteUserRoute user headers
router { route: User user } = userRoute user
router { route: UserValidate uuid } = validateUserRoute uuid
router { route: CheckRequest, headers } = routeCheckRequest headers
router { route: CatchAll paths } = routeError paths

homeRoute :: forall m. MonadAff m => MonadAsk Env m => m Response
homeRoute = do
  ok' corsHeadersAllOrigins ("tunebank 0.1.1")

genreRoute :: forall m. MonadAff m => MonadAsk Env m => m Response
genreRoute = do
  dbpool :: Pool <- asks _.dbpool
  genres :: Array Genre <- liftAff $ withClient dbpool $ do
    getGenres
  let
    json = stringify $ encodeGenres genres
  ok' (jsonHeaders <> corsHeadersAllOrigins) json

rhythmRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> m Response
rhythmRoute genre = do
  dbpool :: Pool <- asks _.dbpool
  rhythms :: Array Rhythm <- liftAff $ withClient dbpool $ do
    getRhythmsForGenre genre
  let
    json = stringify $ encodeRhythms rhythms
  ok' (jsonHeaders <> corsHeadersAllOrigins) json

tunesRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> m Response
tunesRoute genre =
  searchRoute genre defaultSearchParams

searchRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> SearchParams -> m Response
searchRoute genre params = do
  paging :: PagingConfig <- asks _.paging
  let
    searchExpression = buildSearchExpression params
    paginationExpression = buildSearchPaginationExpression params paging.defaultSize
  -- _ <- liftEffect $ logShow searchExpression
  dbpool :: Pool <- asks _.dbpool
  tunesPage :: TuneRefsPage <- liftAff $ withClient dbpool $ do
    getTuneRefsPage genre searchExpression paginationExpression paging.defaultSize
  let
    json = stringify $ encodeTunesPage tunesPage
  ok' (jsonHeaders <> corsHeadersAllOrigins) json

tuneAbcRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> m Response
tuneAbcRoute genre title = do
  dbpool :: Pool <- asks _.dbpool
  mTune <- liftAff $ withClient dbpool $ do
    getTuneAbc genre title
  maybe notFound (ok' (abcHeaders suggestedFileName <> corsHeadersAllOrigins )) mTune

  where
    suggestedFileName = (safeFileName $ show title) <> ".abc"

tuneRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> m Response
tuneRoute genre title = do
  dbpool :: Pool <- asks _.dbpool
  mMetadata :: Maybe TuneMetadata <- liftAff $ withClient dbpool $ do
    getTuneMetadata genre title
  let
    mJson = map (encodeTuneMetadata >>> stringify) mMetadata
  maybe notFound (ok' (jsonHeaders <> corsHeadersAllOrigins)) mJson

tuneMidiRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> m Response
tuneMidiRoute genre title = do
  dbpool :: Pool <- asks _.dbpool
  eMidi <- liftAff $ withClient dbpool $ do
    getTuneMidi genre title
  either customErrorResponse (ok' (midiHeaders suggestedFileName)) eMidi

  where
    suggestedFileName = (safeFileName $ show title) <> ".midi"

deleteTuneRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> RequestHeaders -> m Response
deleteTuneRoute genre title headers = do
  -- _ <- liftEffect $ logShow "deleteTuneRoute"
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  dbpool :: Pool <- asks _.dbpool
  liftAff $ withClient dbpool $ \c -> do
    eAuth :: Either String Authorization <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      eResult <- deleteTune genre title auth.user c
      either customErrorResponse (const $ ok' (corsHeadersOrigin acceptableOrigin) "") eResult

upsertTuneRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> RequestHeaders -> RequestBody -> m Response
upsertTuneRoute genre headers body = do
  dbpool :: Pool <- asks _.dbpool
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  liftAff $ withClient dbpool $ \c -> do
    eAuth :: Either String Authorization <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      abc <- Body.toString body
      case (buildMetadata abc) of
        Left error -> do
          customBadRequest error
        Right validatedAbc -> do
          eResult <- upsertTune genre auth validatedAbc c
          either customErrorResponse (ok' (corsHeadersOrigin acceptableOrigin)) eResult

commentsRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> m Response
commentsRoute genre title = do
  dbpool :: Pool <- asks _.dbpool
  eComments <- liftAff $ withClient dbpool $ do
    getComments genre title
  case eComments of
    Left error ->
      customErrorResponse error
    Right comments -> do
      let
        json = stringify $ encodeComments comments
      ok' (jsonHeaders <> corsHeadersAllOrigins) json

addCommentRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> RequestHeaders -> RequestBody -> m Response
addCommentRoute genre title headers body = do
  -- _ <- liftEffect $ logShow "addCommentRoute"
  dbpool :: Pool <- asks _.dbpool
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  jsonString <- Body.toString body
  {- eNewComment :: Either JsonDecodeError NewComment -}
  case (decodeNewComment jsonString) of
    Left err -> do
      customBadRequest $ printJsonDecodeError err
    Right newComment -> do
      liftAff $ withClient dbpool $ \c -> do
        eAuth :: Either String Authorization <- getAuthorization headers c
        withAnyAuthorization eAuth $ \auth -> do
          eResult <- insertComment genre title newComment auth.user c
          either customErrorResponse (show >>> ok' (corsHeadersOrigin acceptableOrigin)) eResult

commentRoute :: forall m. MonadAff m => MonadAsk Env m => Int -> m Response
commentRoute commentId = do
  dbpool :: Pool <- asks _.dbpool
  eComment <- liftAff $ withClient dbpool $ do
    getComment commentId
  case eComment of
    Left error ->
      customErrorResponse error
    Right comment -> do
      let
        json = stringify $ encodeComment comment
      ok' (jsonHeaders <> corsHeadersAllOrigins) json

deleteCommentRoute :: forall m. MonadAff m => MonadAsk Env m => Int -> RequestHeaders -> m Response
deleteCommentRoute commentId headers = do
  dbpool :: Pool <- asks _.dbpool
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  liftAff $ withClient dbpool $ \c -> do
    eAuth :: Either String Authorization <- getAuthorization headers c
    withAnyAuthorization eAuth $ \auth -> do
      eResult <- deleteComment commentId auth c
      either customErrorResponse (const $ ok' (corsHeadersOrigin acceptableOrigin) "") eResult

deleteTuneCommentsRoute :: forall m. MonadAff m => MonadAsk Env m => Genre -> Title -> RequestHeaders -> m Response
deleteTuneCommentsRoute genre title headers = do
  dbpool :: Pool <- asks _.dbpool
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  liftAff $ withClient dbpool $ \c -> do
    eAuth :: Either String Authorization <- getAuthorization headers c
    withAdminAuthorization eAuth $ \_auth -> do
      _result <- deleteComments genre title c
      ok' (corsHeadersOrigin acceptableOrigin) ""

updateCommentRoute :: forall m. MonadAff m => MonadAsk Env m => Int -> RequestBody -> RequestHeaders -> m Response
updateCommentRoute id body headers = do
  -- _ <- liftEffect $ logShow ("update comment " <> show id)
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  jsonString <- Body.toString body
  {- eNewComment :: Either JsonDecodeError NewComment -}
  case (decodeNewComment jsonString) of
    Left err ->
      customBadRequest $ printJsonDecodeError err
    Right updatedComment -> do
      dbpool :: Pool <- asks _.dbpool
      liftAff $ withClient dbpool $ \c -> do
        eAuth :: Either String Authorization <- getAuthorization headers c
        withAnyAuthorization eAuth $ \auth -> do
          eResult <- updateComment id updatedComment auth c
          either customErrorResponse (show >>> ok' (corsHeadersOrigin acceptableOrigin)) eResult

usersRoute :: forall m. MonadAff m => MonadAsk Env m => PagingParams -> RequestHeaders -> m Response
usersRoute pagingParams headers = do
  paging :: PagingConfig <- asks _.paging
  -- _ <- liftEffect $ log ("user paging params: " <> show pagingParams)
  let
    paginationExpression = buildPaginationExpression pagingParams paging.defaultSize
    mOrigin = lookup headers "origin"
  -- _ <- liftEffect $ log $ "origin: " <> show mOrigin
  acceptableOrigin <- validateCorsOrigin mOrigin
  dbpool :: Pool <- asks _.dbpool
  liftAff $ withClient dbpool $ \c -> do
    eAuth <- getAuthorization headers c
    withAdminAuthorization eAuth $ \_auth -> do
      usersPage <- getUserRecordsPage paginationExpression paging.defaultSize c
      let
        json = stringify $ encodeUserRecordsPage usersPage
      ok' (jsonHeaders <> corsHeadersOrigin acceptableOrigin) json

checkUserRoute :: forall m. MonadAff m => MonadAsk Env m => RequestHeaders -> m Response
checkUserRoute headers = do
  -- _ <- liftEffect $ log "checkUserRoute"
  dbpool :: Pool <- asks _.dbpool
  logger <- asks _.logger
  liftAff $ withClient dbpool $ \c -> do
    eAuth <- getAuthorization headers c
    -- _ <- liftEffect $ logShow eAuth
    case eAuth of
      Right auth -> do
        liftEffect $ logInfo logger $ "login for " <> show auth.user
        pure unit
      Left err -> do
        liftEffect $ logError logger $ "login failure " <> err
        pure unit
    either (const unauthorized) (\auth -> ok' corsHeadersAllOrigins auth.role) eAuth

userRoute :: forall m. MonadAff m => MonadAsk Env m => UserName -> m Response
userRoute user = do
  -- _ <- liftEffect $ log "userRoute"
  dbpool :: Pool <- asks _.dbpool
  mUser <- liftAff $ withClient dbpool $ do
    getUserRecord user
  let
    mJson = map (stringify <<< encodeUserRecord) mUser
  maybe notFound (ok' (jsonHeaders <> corsHeadersAllOrigins)) mJson

insertUserRoute :: forall m. MonadAff m => MonadAsk Env m => RequestBody -> m Response
insertUserRoute body = do
  jsonString <- Body.toString body
  {- eNewUser :: Either JsonDecodeError NewUser -}
  case (decodeNewUser jsonString) of
    Left err ->
      customBadRequest $ printJsonDecodeError err
    Right newUser -> do
      dbpool :: Pool <- asks _.dbpool
      eResult <- liftAff $ withClient dbpool $ do
        insertUser newUser Unvalidated

      case eResult of
        Right uuid -> do
          emailResult <- sendRegistrationMail newUser.email uuid
          case emailResult of
            Left _error -> do
              -- we got an email error so delete the user we just added
              _ <- liftAff $ withClient dbpool $ do
                deleteUser $ UserName newUser.name
              internalServerError ("Server failed to send a registration email to " <> newUser.email)
            Right _info ->
              ok' corsHeadersAllOrigins ("User: " <> newUser.name <> " created.")
        Left err -> do
          customErrorResponse err

deleteUserRoute :: forall m. MonadAff m => MonadAsk Env m => UserName -> RequestHeaders -> m Response
deleteUserRoute user headers = do
  -- _ <- liftEffect $ log "deleteUserRoute"
  dbpool :: Pool <- asks _.dbpool
  liftAff $ withClient dbpool $ \c -> do
    eAuth :: Either String Authorization <- getAuthorization headers c
    withAdminAuthorization eAuth $ \_auth -> do
      _result <- deleteUser user c
      ok' corsHeadersAllOrigins ""

validateUserRoute :: forall m. MonadAff m => MonadAsk Env m => String -> m Response
validateUserRoute uuid = do
  dbpool :: Pool <- asks _.dbpool
  _ <- liftAff $ withClient dbpool $ do
    validateUser uuid
  ok' corsHeadersAllOrigins "validated"

-- | Receive the One-Time-Password UUID from the request body and email it to the user if we find her
userNewPasswordOTPRoute :: forall m. MonadAff m => MonadAsk Env m => RequestBody -> m Response
userNewPasswordOTPRoute body = do
  jsonString <- Body.toString body
  case (decodeUserPasswordOTP jsonString) of
    Left err -> do
      customBadRequest $ printJsonDecodeError err
    Right userPasswordOTP -> do
      dbpool :: Pool <- asks _.dbpool
      mUserRecord <- liftAff $ withClient dbpool $ do
        getUserRecord (UserName userPasswordOTP.name)

      case mUserRecord of
        Just userRecord -> do
          emailResult <- sendNewPasswordOTPMail userRecord.email userPasswordOTP.otp
          either
            customErrorResponse
            (const $ ok' corsHeadersAllOrigins ("A one-time-password (OTP) has been sent to " <> userRecord.email <> "."))
            emailResult

        Nothing ->
          customBadRequest "User not found"

-- | Change the user password.  The frontend is responsible for determining if the OTP password has been validated
userNewPasswordRoute :: forall m. MonadAff m => MonadAsk Env m => RequestBody -> m Response
userNewPasswordRoute body = do
  jsonString <- Body.toString body
  case (decodeUserPassword jsonString) of
    Left err -> do
      -- _ <- liftEffect $ logShow $ "Error decoding JSON " <> (show err)
      customBadRequest $ printJsonDecodeError err
    Right userPassword -> do
      dbpool :: Pool <- asks _.dbpool
      liftAff $ withClient dbpool $ \c -> do
        changeUserPassword (UserName userPassword.name) userPassword.password c
        ok' corsHeadersAllOrigins ("User: " <> userPassword.name <> " password updated.")

-- | Get the user name from the email address
userGetNameRoute :: forall m. MonadAff m => MonadAsk Env m => RequestBody -> m Response
userGetNameRoute body = do
  email <- Body.toString body
  dbpool :: Pool <- asks _.dbpool
  mUserName <- liftAff $ withClient dbpool $ do
    getUserName email
  case mUserName of
    Just userName -> do
      emailResult <- sendUserNameMail email userName
      either
        customErrorResponse
        (const $ ok' corsHeadersAllOrigins ("user name emailed to user: " <> userName))
        emailResult
    Nothing ->
      customBadRequest "Email not found"

preflightOptionsRoute :: forall m. MonadAff m => MonadAsk Env m => RequestHeaders -> m Response
preflightOptionsRoute headers = do
  let
    mOrigin = lookup headers "origin"
  acceptableOrigin <- validateCorsOrigin mOrigin
  ok' (preflightOrigin acceptableOrigin) ""

routeError :: forall m. MonadAff m => MonadAsk Env m => Array String -> m Response
routeError paths = do
  let
    fullPath = intercalate "/" paths
  ok' corsHeadersAllOrigins ("we got a routing error: " <> fullPath)

routeCheckRequest :: forall m. MonadAff m => MonadAsk Env m => RequestHeaders -> m Response
routeCheckRequest headers = do
  ok ("request headers: " <> show headers)

