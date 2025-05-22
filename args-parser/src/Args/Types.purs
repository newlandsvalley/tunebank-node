module Args.Types 
  ( IncomingGenre(..)
  , Target(..)
  ) where

import Prelude

data IncomingGenre =
    English 
  | Irish 
  | Klezmer 
  | Scandi
  | Scottish

instance showIncomingGenre :: Show IncomingGenre where
  show English = "english"
  show Irish = "irish"
  show Klezmer = "klezmer"
  show Scandi = "scandi"
  show Scottish = "scottish"

derive instance eqIncomingGenre :: Eq IncomingGenre
derive instance ordIncomingGenre :: Ord IncomingGenre


data Target = 
    Users 
  | Tunes IncomingGenre
  | Comments IncomingGenre

derive instance eqTarget :: Eq Target
derive instance ordTargt :: Ord Target

instance showTarget :: Show Target where 
  show Users = "users"
  show (Tunes g) = show g <> " " <> "tunes"
  show (Comments g) = show g <> " " <> "comments"
