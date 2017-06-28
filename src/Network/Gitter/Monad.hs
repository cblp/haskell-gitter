module Network.Gitter.Monad where

import           Network.Gitter.Types

import           Control.Monad.Reader
import           Data.Aeson

class Monad m => MonadGitter m where
    runGitterAction :: ResourcePath -> Value -> m Value

instance MonadGitter m => MonadGitter (ReaderT r m) where
    runGitterAction path = lift . runGitterAction path
