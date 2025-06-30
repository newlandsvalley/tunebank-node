module Test.Database (databaseSpec, rebuildDB) where



import Data.Array (length)
import Data.Bifunctor (rmap)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), isJust)
import Data.String.CodeUnits (length) as STRING
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (<>))
import Test.Spec (Spec, before_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Utils (adminAuth, fastan, generatePostgresTimestamp, getInitialCommentId, getRegistrationId, withDBConnection)
import Tunebank.Database.Comment (getComments, deleteComment, deleteComments, insertComment, updateComment)
import Tunebank.Database.Genre (existsGenre, getGenreStrings)
import Tunebank.Database.Rhythm (existsRhythm, getRhythmStrings)
import Tunebank.Database.Search (SearchCriterion(..), SearchOperator(..), buildSearchExpressionString)
import Tunebank.Database.Tune (countSelectedTunes, getTuneMetadata, getTuneAbc, getTuneRefs)
import Tunebank.Database.User (UserValidity(..), deleteUser, changeUserPassword, getUserPassword, existsUser, getUserRecord, getUserRecords, getUserRole, insertUser, validateCredentials, validateUser)
import Tunebank.HTTP.Response (ResponseError(..))
import Tunebank.Logic.Api (upsertValidatedTuneWithTs)
import Tunebank.Pagination (defaultPaginationExpression)
import Tunebank.Tools.Loader (uploadTunes)
import Tunebank.Types (Genre(..), Rhythm(..), Title(..), UserName(..), NewUser, Role(..))
import Yoga.Postgres (Client, Query(Query), execute_)


databaseSpec :: Spec Unit
databaseSpec = do
    userSpec
    genreSpec
    rhythmSpec
    tuneSpec
    commentSpec
    searchSpec

userSpec :: Spec Unit
userSpec = before_ flushUsers do
  describe "users table" do
    it "finds role by user name" do
      res <- withDBConnection $ getUserRole (UserName "administrator")
      res `shouldEqual` (Just (Role "administrator"))
    it "returns role - nothing if unknown user " do
      res <- withDBConnection $ getUserRole (UserName "NotAKnownUser")
      res `shouldEqual` Nothing
    it "finds an existing user name" do
      res <- withDBConnection $ existsUser (UserName "John")
      res `shouldEqual` true
    it "returns false if user doesn't exist " do
      res <- withDBConnection $ existsUser (UserName "NotAKnownUser") 
      res `shouldEqual` false
    it "finds an existing user record" do
      res <- withDBConnection $ getUserRecord (UserName "John")
      map (_.username) res `shouldEqual` Just (UserName "John")
      map (_.role) res `shouldEqual` Just (Role "normaluser")
    it "finds Nothing for a user record if the user doesn't exist" do
      res <- withDBConnection $ getUserRecord (UserName "NotAKnownUser")
      res `shouldEqual` Nothing
    it "finds all users" do
      res <- withDBConnection $ getUserRecords defaultPaginationExpression
      length res `shouldEqual` 5
    it "inserts a new (as yet unregistered) user" do
      let 
        newUser :: NewUser
        newUser = { name: "NewUser", password: "changeit", email: "newuser@google.com" }
      -- this should return Right (UUID String) of 36 characters
      res <- withDBConnection do
        insertUser newUser Unvalidated
      let 
        lengthRes = rmap STRING.length res      
      lengthRes `shouldEqual` (Right 36)
    it "does not insert a new user if the user name is taken" do
      let 
        newUser :: NewUser
        newUser = { name: "John", password: "changeit", email: "john@google.com" }     
      res <- withDBConnection do
        insertUser newUser Unvalidated
      res `shouldEqual` Left (BadRequest ("username " <> newUser.name <> " is already taken"))
    it "validates a user" do  
      withDBConnection $ \c -> do 
        let 
          userName = "Jim"
        uuid <- getRegistrationId userName c
        _ <- validateUser uuid c
        -- registration should set user.valid to 'Y'
        mUser <- getUserRecord (UserName userName) c
        let 
          validity = map _.valid mUser
        validity `shouldEqual` Just "Y" 
    it "validates user credentials" do
      let 
        credentials = { user: "John", password: "changeit" }
      res <- withDBConnection do
        validateCredentials credentials
      res `shouldEqual` Right ( { user: UserName "John", role : Role "normaluser" } )
    it "changes a user password" do
      withDBConnection \c -> do
        let 
          newUser :: NewUser
          newUser = { name: "KeirStarmer", password: "changeit", email: "keirstarmer@google.com" }
          user = UserName "KeirStarmer"
        _ <- insertUser newUser Prevalidated c
        _ <- changeUserPassword user "NewPassword" c
        mPassword <- getUserPassword user c
        mPassword `shouldEqual` Just "NewPassword"
    
genreSpec :: Spec Unit
genreSpec =
  describe "genre table" do
    it "finds the scandi genre" do
      res <- withDBConnection $ existsGenre (Genre "scandi") 
      res `shouldEqual` true
    it "retrieves all the genres" do
      genres <- withDBConnection $ getGenreStrings
      length genres `shouldEqual` 5

rhythmSpec :: Spec Unit
rhythmSpec =
  describe "rhythm table" do
    it "finds rhythm by genre and rhythm" do
      res <- withDBConnection $ existsRhythm (Genre "scandi") (Rhythm "brudmarsch")
      res `shouldEqual` true
    it "retrieves all the scandi rhythms" do
      genres <- withDBConnection $ getRhythmStrings (Genre "scandi")
      length genres `shouldEqual` 14

tuneSpec :: Spec Unit
tuneSpec =
  describe "tune table" do
    it "counts all scandi tunes" do
      res <- withDBConnection $ countSelectedTunes (Genre "scandi")  []
      (show res)`shouldEqual` "21"
    it "counts all scandi polskas" do
      res <- withDBConnection $ countSelectedTunes (Genre "scandi")  [{ criterion: ByRhythm, operator: Equals, target: "polska"}]
      (show res) `shouldEqual` "13"
    {- but why does this fail? with 13 not equal to 13 ?
      res <- withDBConnection $ countSelectedTunes (Genre "scandi")  [{ criterion: ByRhythm, operator: Equals, target: "polska"}]
      res `shouldEqual` 13
    -}
    it "counts all scandi tunes in Gm" do
      res <- withDBConnection $ countSelectedTunes (Genre "scandi")  [{ criterion: ByKeySig, operator: Equals, target: "GMinor"}]
      (show res) `shouldEqual` "5"
    it "counts all scandi polskas in Gm" do
      res <- withDBConnection $ countSelectedTunes (Genre "scandi")  
        [ { criterion: ByRhythm, operator: Equals, target: "polska"}
        , { criterion: ByKeySig, operator: Equals, target: "GMinor"}]
      (show res) `shouldEqual` "4"
    it "counts all scandi tunes by Bruun" do
      res <- withDBConnection $ countSelectedTunes (Genre "scandi")  [{ criterion: ByTitle, operator: Like, target: "bruun"}]
      (show res) `shouldEqual` "2"
    it "returns references to any schottis in AMajor" do
      res <- withDBConnection $ getTuneRefs (Genre "scandi")  
         [ { criterion: ByRhythm, operator: Equals, target: "schottis"}
         , { criterion: ByKeySig, operator: Equals, target: "AMajor"} ]
         defaultPaginationExpression
      length res `shouldEqual` 2
    it "finds tune metadata" do
      mMetadata <- withDBConnection $ getTuneMetadata (Genre "scandi") (Title "Elverumspols")
      case mMetadata of 
        Nothing -> 
           fail "no matching tune found"
        Just metadata -> do
           metadata.title `shouldEqual` (Title "Elverumspols")
           metadata.composer `shouldEqual` Nothing
           metadata.submitter `shouldEqual` (UserName "John")
      pure unit
    it "finds tune abc" do
      result <- withDBConnection $ getTuneAbc (Genre "scandi") (Title "Elverumspols") 
      result `shouldSatisfy` isJust
      pure unit
    it "upserts a tune with a client-generated timestamp" do
      ts <- liftEffect generatePostgresTimestamp
      result <- withDBConnection $ \c -> do 
        upsertValidatedTuneWithTs adminAuth c (Genre "scandi") ts fastan
      result `shouldEqual` (Right "Fastan")
      pure unit

commentSpec :: Spec Unit
commentSpec =
  describe "comment table" do
    it "inserts a new comment" do
      let 
        newComment = 
          { subject : "This is my scathing comment"
          , text : "This is a horrible tune"
          }
      res <- withDBConnection do
        insertComment (Genre "scandi") (Title "Elverumspols") newComment (UserName "John")
      res `shouldSatisfy` isRight

    it "gets all comment for a tune" do
      res <- withDBConnection do 
        getComments (Genre "scandi") (Title "Griffenfeldt")
      -- res `shouldSatisfy` isRight
      case res of 
        Left err -> 
          fail $ show err 
        Right comments ->
          (length comments) `shouldEqual` 2
    it "updates a comment" do
      res <- withDBConnection $ \c -> do
        commentId <- getInitialCommentId c
        let 
          updatedComment = { subject : "This is new comment 1"
                           , text : "This is new text 1"             
                           }
          auth = { user : (UserName "administrator")
                 , role : (Role "administrator")
                 }
        updateComment commentId updatedComment auth c

      res `shouldSatisfy` isRight
    it "bars a comment update from an unauthorised user" do
      res <- withDBConnection $ \c -> do
        commentId <- getInitialCommentId c
        let 
          updatedComment = { subject : "This is Bert's new comment 1"
                           , text : "This is Bert's new text 1"           
                           }
          auth = { user : (UserName "Bert")
                 , role : (Role "normaluser")
                 }
        updateComment commentId updatedComment auth c
      res `shouldEqual` Left (Forbidden "Only the original comment submitter or an administrator can update a comment")
      
    it "bars a comment delete from an unauthorised user" do
    
      res <- withDBConnection $ \c -> do
        commentId <- getInitialCommentId c
        deleteComment commentId ( { user: (UserName "Bert"), role: (Role "normaluser") }) c 

      res `shouldEqual` Left (Forbidden "Only the original comment submitter or an administrator can delete a comment")
    it "allows a comment delete from the original submitter" do
      withDBConnection $ \c -> do
        commentId <- getInitialCommentId c 
        eRes <- deleteComment commentId ( { user: (UserName "John"), role: (Role "normaluser") }) c
        case eRes of 
          Left err -> fail $ show err
          Right _ -> do
            eComments <- getComments (Genre "scandi") (Title "Griffenfeldt") c
            case eComments of 
              Left err1 -> fail $ show err1
              Right comments -> 
                length comments `shouldEqual` 1
      

searchSpec :: Spec Unit 
searchSpec = 
  describe "building search expressions" do
    it "builds a simple search" do 
      let 
         expression = [ { criterion: ByTitle, operator: Like, target:"Fastan"}
                      , { criterion: ByRhythm, operator: Equals, target:"polska"}
                      , { criterion: ByKeySig, operator: Equals, target:"BMinor"}
                      ]
      buildSearchExpressionString expression `shouldEqual` 
        " and title ilike '%Fastan%' and rhythm = 'polska' and keysig = 'BMinor'"
  

-- the tunes table in the database for the scandi genre is completely rebuilt just once before any tests run
-- and we also add a couple of comments on one of the tunes
rebuildDB :: Aff Unit 
rebuildDB = do 
  _ <- liftEffect $ log "REBUILDING THE DATABASE"
  withDBConnection $ \c -> do 
    -- delete all comments
    _ <- deleteAllComments c
    -- delete all Scandi tunes
    _ <- deleteAllScandiTunes c
    -- upload the Scandi tunes in the abc-samples directory
    _ <- uploadTunes (Genre "scandi") "./test-data/abc-samples" c
    insertTwoComments c

-- to be run before the user tests
flushUsers :: Aff Unit 
flushUsers= do 
  withDBConnection \c -> do 
    -- delete any existing new user from previous test runs
    deleteUser (UserName "NewUser") c
    deleteUser (UserName "Albert") c
    deleteUser (UserName "KeirStarmer") c
    deleteUser (UserName "TonyBlair") c
    -- prevalidate Tony Blair used for change password integration tests
    let 
      tonyBlair :: NewUser
      tonyBlair = { name: "TonyBlair", password: "changeit", email: "tony@blairfoundation.com" }     
    _ <- insertUser tonyBlair Prevalidated c
    pure unit

deleteAllScandiTunes :: Client -> Aff Unit 
deleteAllScandiTunes c = do
  execute_ (Query "delete from tunes where genre = 'scandi' ") c

{-}
deleteAllComments :: Client -> Aff Unit 
deleteAllComments c = do
  execute_ (Query "delete from comments ") c
-}

-- if we use this, then it tests the database API rather than just a blanket delete
deleteAllComments :: Client -> Aff Unit 
deleteAllComments c = do
  -- _ <- liftEffect $ log "DELETING COMMENTS FROM griffenfeldt, elverumspols and getingen"
  _ <- deleteComments (Genre "scandi") (Title "Griffenfeldt") c
  _ <- deleteComments (Genre "scandi") (Title "Elverumspols") c
  _ <- deleteComments (Genre "scandi") (Title "Getingen") c
  pure unit


-- insert a pair of comments against the griffenfeldt tune
insertTwoComments :: Client -> Aff Unit
insertTwoComments c = 
  let 
    newComment1 = 
      { subject : "This is comment 1"
      , text : "This is the text of comment 1"
      }
    newComment2 = 
      { subject : "This is comment 2"
      , text : "This is the text of comment 2"
      }  
  in do
    _ <- insertComment (Genre "scandi") (Title "Griffenfeldt") newComment1 (UserName "John") c
    _ <- insertComment (Genre "scandi") (Title "Griffenfeldt") newComment2 (UserName "John") c
    pure unit
  

        
      
