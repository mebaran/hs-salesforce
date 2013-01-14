{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}

module Network.Salesforce.Types where

import Data.ByteString (ByteString)

import Control.Failure
import Network.HTTP.Conduit
import Control.Monad.Reader
import Control.Applicative

import Data.Conduit
import Data.Aeson


data SFToken = SFToken { issuedAt    :: ByteString,
                         instanceURL :: ByteString,
                         accessToken :: ByteString,
                         signature   :: ByteString } deriving Show

instance FromJSON SFToken where
  parseJSON (Object v) = SFToken <$> (v .: "issued_at") <*>
                         (v .: "instance_url") <*>
                         (v .: "access_token") <*>
                         (v .: "signature")
  parseJSON _ = fail "Invalid login request."

type SFContext m = MonadReader (Manager, SFToken) m

type RestIO m = (MonadBaseControl IO m, MonadResource m, Failure HttpException m)
type SFConnection m a = ReaderT (Manager, SFToken) (ResourceT m) a
type SFRunnable m = (MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m) 
