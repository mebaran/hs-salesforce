{-# LANGUAGE OverloadedStrings #-}

module Network.Salesforce.Types where

import Data.ByteString

import Control.Applicative
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
