{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}

module Network.Salesforce.Types where

import Data.Text as T
import Data.ByteString as BS

import Control.Monad.Reader
import Control.Applicative

import Network.HTTP.Conduit (Manager)
import Data.Aeson


data SFToken = SFToken { issuedAt    :: BS.ByteString,
                         instanceURL :: BS.ByteString,
                         accessToken :: BS.ByteString,
                         signature   :: BS.ByteString } deriving Show

instance FromJSON SFToken where
  parseJSON (Object v) = SFToken <$> (v .: "issued_at") <*>
                         (v .: "instance_url") <*>
                         (v .: "access_token") <*>
                         (v .: "signature")
  parseJSON _ = fail "Invalid login request."

type SFContext m = MonadReader (Manager, SFToken) m

data SObject a = SObject { sobjectClass :: Text,
                           sobjectId :: Text,
                           sobjectValues :: [(Text, a)]}

