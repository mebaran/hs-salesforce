{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
module Network.Salesforce.Util where

import Data.Time.Format
import System.Locale

import Data.Monoid
import qualified Data.ByteString.UTF8 as BSU

import Control.Failure
import Control.Monad.Reader
import Control.Monad.Trans.Control  (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource)

import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Conduit
import Network.HTTP.Types.Method (Method)

import Data.Aeson (FromJSON, Result(..), Value, fromJSON, json)

import Network.Salesforce.Types

jsonUnpack :: (Monad m, FromJSON a) => Value -> m a
jsonUnpack ju = case fromJSON ju of
                  Success obj -> return obj
                  Error err -> fail err

httpJSON :: (MonadBaseControl IO m, MonadResource m) => Request m -> Manager -> m Value
httpJSON req mgr = do
  Response _ _ _ body <- http req mgr
  body $$+- sinkParser json

readSFTime :: ParseTime t => String -> t
readSFTime = readTime defaultTimeLocale "%F"

sfReq :: (SFContext m, Failure HttpException m) => Method -> BSU.ByteString -> m (Request m1)
sfReq method path = do
  (_, token) <- ask
  req <- parseUrl $ BSU.toString $ mconcat [instanceURL token, path]
  return req{method = method, requestHeaders = [("Authorization", mappend "Bearer " $ accessToken token)]}

sfAction :: (SFContext m, MonadBaseControl IO m, MonadResource m) => Request m -> m Value
sfAction req = liftM fst ask >>= httpJSON req

sfJSON :: (MonadBaseControl IO m, MonadResource m, SFContext m, FromJSON b) => Request m -> m b
sfJSON req = sfAction req >>= jsonUnpack

sfGet :: (SFContext m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfGet = sfReq "GET"

sfPost :: (SFContext m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfPost = sfReq "POST"

sfPut :: (SFContext m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfPut = sfReq "PUT"

sfDelete :: (SFContext m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfDelete = sfReq "DELETE"

sfPatch :: (SFContext m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfPatch = sfReq "PATCH"
