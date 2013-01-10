{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Network.Salesforce where

import Data.Monoid
import Control.Applicative
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

import Control.Failure
import Control.Monad.Trans.Control  (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, MonadUnsafeIO, MonadThrow)
import Data.Conduit                 (runResourceT, ($$+-))
import Data.Conduit.Attoparsec      (sinkParser)
import Network.HTTP.Conduit
import Network.HTTP.Types.Method    (Method)

import Data.Aeson

import Data.Time.Format
import System.Locale

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

connectSF :: (MonadBaseControl IO m, MonadResource m, Failure HttpException m) => BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Manager -> m SFToken
connectSF clientID clientSecret username password securityToken mgr = do
  req <- parseUrl "https://login.salesforce.com/services/oauth2/token"
  let bodyreq = urlEncodedBody [("grant_type", "password"),
                                ("client_id", clientID), ("client_secret", clientSecret),
                                ("username", username),
                                ("password", mconcat [password, securityToken])] req
  jsonUnpack =<< httpJSON bodyreq mgr

sfReq :: (MonadReader (t, SFToken) m, Failure HttpException m) => Method -> BSU.ByteString -> m (Request m1)
sfReq method path = do
  (_, token) <- ask
  req <- parseUrl $ BSU.toString $ mconcat [instanceURL token, path]
  return req{method = method, requestHeaders = [("Authorization", mappend "Bearer " $ accessToken token)]}

sfAction :: (MonadReader (Manager, t) m, MonadBaseControl IO m, MonadResource m) => Request m -> m Value
sfAction req = do
  (mgr, _) <- ask
  httpJSON req mgr

sfGet :: (MonadReader (t, SFToken) m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfGet = sfReq "GET"
sfPost :: (MonadReader (t, SFToken) m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfPost = sfReq "POST"
sfPut :: (MonadReader (t, SFToken) m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfPut = sfReq "PUT"
sfDelete :: (MonadReader (t, SFToken) m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfDelete = sfReq "DELETE"
sfPatch :: (MonadReader (t, SFToken) m, Failure HttpException m) => BSU.ByteString -> m (Request m1)
sfPatch = sfReq "PATCH"

data QueryCursor a = QueryCursor { records :: [a], nextRecordsUrl :: Maybe BS.ByteString } deriving Show

instance FromJSON a => FromJSON (QueryCursor a) where
    parseJSON (Object v) = QueryCursor <$> (v .: "records") <*> (v .:? "nextRecordsUrl")
    parseJSON _ = fail "Invalid query result."

sfQuery :: (MonadReader (Manager, SFToken) m, MonadBaseControl IO m, MonadResource m, Failure HttpException m, FromJSON b) => BSU.ByteString -> m b
sfQuery q = do
  query <- sfGet (mappend "/services/data/v26.0/query?q=" q)
  js <- sfAction $ query{responseTimeout = Nothing}
  jsonUnpack js

sfQueryMore :: (MonadReader (Manager, SFToken) m, MonadBaseControl IO m, MonadResource m, Failure HttpException m, FromJSON b) => BSU.ByteString -> m b
sfQueryMore nexturl = do
  req <- sfGet nexturl
  js <- sfAction req
  jsonUnpack js

sfQueryAll :: 
    (MonadReader (Manager, SFToken) m, MonadBaseControl IO m, MonadResource m, Failure HttpException m, FromJSON b) =>
    BSU.ByteString -> m [b]
sfQueryAll qs = do
  fq <- sfQuery qs
  rest [fq] fq
  where 
    rest acc q = case nextRecordsUrl q of
                   Just url -> do
                     nextq <- sfQueryMore url
                     rest (nextq:acc) nextq
                   Nothing -> return $ concatMap records acc
    

runSF :: MonadBaseControl IO m => Manager -> SFToken -> ReaderT (Manager, SFToken) (ResourceT m) a -> m a
runSF mgr token runner = runResourceT $ runReaderT runner (mgr, token)

runAuthSF :: 
    MonadBaseControl IO m => 
   (Manager -> m SFToken) -> ReaderT (Manager, SFToken) (ResourceT m) b -> Manager -> m b
runAuthSF auth action mgr = do
  token <- auth mgr
  runSF mgr token action

runSingleAuthSF ::
  (MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m) =>
  (Manager -> ResourceT m SFToken) -> ReaderT (Manager, SFToken) (ResourceT (ResourceT m)) a -> m a
runSingleAuthSF login action = withManager $ \mgr -> runAuthSF login action mgr


