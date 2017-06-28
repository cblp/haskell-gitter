module Gitter.Monad (MonadGitter (..)) where

import Control.Monad.Reader (ReaderT, lift)
import Data.Aeson (Value)

import Gitter.Types (ResourcePath)

class Monad m => MonadGitter m where
    runGitterAction :: ResourcePath -> Value -> m Value

instance MonadGitter m => MonadGitter (ReaderT r m) where
    runGitterAction path = lift . runGitterAction path
