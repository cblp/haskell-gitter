{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Gitter
    ( Gitter(..)
    , GitterT
    , runGitterT
    , sendChatMessage
    , withRoom
    ) where

import           Control.Lens           ((&~), (?=), (^.), (^?))
import           Control.Monad          (void)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Trans    (MonadTrans, lift)
import           Data.Aeson             (Value (String), object)
import           Data.Aeson.Lens        (key, _String)
import qualified Data.ByteString.Char8  as ByteString
import qualified Data.List              as List
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Network.Wreq           (asJSON, auth, defaults, oauth2Bearer,
                                         postWith, responseBody)

import           Network.Gitter.Monad   (MonadGitter (runGitterAction))
import           Network.Gitter.Types   (Gitter (..), ResourcePath,
                                         Room (ONETOONE, REPO), RoomId, RoomUri)

newtype GitterT m a = GitterT (ReaderT Gitter m a)
    deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadTrans)

runGitterT :: Gitter -> GitterT m a -> m a
runGitterT gitter (GitterT readerAction) = runReaderT readerAction gitter

newtype GitterRoomT m a = GitterRoomT (ReaderT Room m a)
    deriving Functor

roomUri :: Room -> RoomUri
roomUri (ONETOONE user)  = user
roomUri (REPO user repo) = user <> "/" <> repo

withRoom :: Room -> GitterRoomT gitter a -> gitter a
withRoom room (GitterRoomT readerAction) =
    runReaderT readerAction room

sendChatMessage :: MonadGitter m => Text -> GitterRoomT m ()
sendChatMessage text =
    void . runRoomAction ["chatMessages"] $ object [("text", String text)]

runRoomAction :: MonadGitter m => ResourcePath -> Value -> GitterRoomT m Value
runRoomAction path request = GitterRoomT $ do
    room <- ask
    roomId <- lift $ joinRoom room
    lift $ runGitterAction (["rooms", roomId] <> path) request

joinRoom :: MonadGitter m => Room -> m RoomId
joinRoom room = do
    jsonResponse <- runGitterAction ["rooms"] $
        object [("uri", String $ roomUri room)]
    maybe (fail "joining room must return a string \"id\"") return $
        jsonResponse ^? key "id" . _String

instance (MonadIO io, MonadThrow io) => MonadGitter (GitterT io) where
    runGitterAction path apiRequest = GitterT $ do
        Gitter{gitter_baseUrl, gitter_tokenFile} <- ask
        tokenFileContents <- liftIO $ ByteString.readFile gitter_tokenFile
        let token = normalizeSpace tokenFileContents
            url = List.intercalate "/" (gitter_baseUrl : fmap Text.unpack path)
            opts = defaults &~ auth ?= oauth2Bearer token
        response <- liftIO (postWith opts url apiRequest)
        jsonResponse <- asJSON response
        return (jsonResponse ^. responseBody)
      where
        normalizeSpace = ByteString.unwords . ByteString.words
