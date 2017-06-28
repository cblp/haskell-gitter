{-# LANGUAGE TemplateHaskell #-}

module Network.Gitter.Types where

import           Data.Aeson.TH (SumEncoding (TaggedObject), contentsFieldName,
                                defaultOptions, deriveJSON, fieldLabelModifier,
                                sumEncoding, tagFieldName)
import           Data.String.X (dropPrefix)
import           Data.Text     (Text)

type ResourcePath = [Text]
type UserName = Text
type RepoName = Text
type RoomId = Text
type RoomUri = Text

data Room
    = ONETOONE UserName
    | REPO UserName RepoName
    deriving Show

deriveJSON
    defaultOptions
        { sumEncoding =
              TaggedObject{tagFieldName = "type", contentsFieldName = "uri"}
        }
    ''Room

data Gitter = Gitter
    { gitter_baseUrl   :: String
    , gitter_room      :: Room
    , gitter_tokenFile :: FilePath
    }

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "gitter_"} ''Gitter
