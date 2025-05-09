module Types (IncomingGenre(..)) where

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


