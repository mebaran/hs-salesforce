{-# LANGUAGE FlexibleContexts, ConstraintKinds, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Network.Salesforce.Bulk where

import Data.Maybe
import Data.Monoid
import Control.Monad.Reader
import Control.Failure
import Control.Applicative

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BSU
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

import Data.Conduit
import Network.HTTP.Types
import Network.HTTP.Conduit

import Text.XML
import Text.XML.Cursor
import Text.Hamlet.XML

import Data.Csv

import Network.Salesforce.Types

data JobResult = JobResult T.Text Bool Bool T.Text

instance FromField Bool where
    parseField s = case BSU.toString s of
                     c:_ -> return $ c == 't' || c == 'T'
                     _ -> return False

instance FromRecord JobResult where
    parseRecord v = if V.length v >= 4
                    then JobResult <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
                    else fail "Not long enough"

httpXML :: (RestIO m, SFContext m) => Request m -> m Document
httpXML req = do
  (mgr, _) <- ask
  Response _ _ _ body <- http req mgr
  body $$+- sinkDoc def

bulkReq :: (SFContext m, Failure HttpException m) => Method -> BSU.ByteString -> m (Request m1)
bulkReq method path = do
  (_, token) <- ask
  req <- parseUrl $ BSU.toString $ mconcat [instanceURL token, path]
  return req{method = method, requestHeaders = [("X-SFDC-Session", accessToken token)]}

bulkPost :: (Failure HttpException m, SFContext m) => BSU.ByteString -> m (Request m1)
bulkPost = bulkReq "POST"

type Object = T.Text
data Operation = Insert | Upsert | Update | Delete

opText :: Operation -> T.Text
opText Insert = "insert"
opText Upsert = "upsert"
opText Update = "update"
opText Delete = "delete"

registerJob :: (RestIO m, SFContext m) => Operation -> Object -> m Document
registerJob operation object = do
  post <- bulkPost "/services/async/v26.0/job"
  let body = RequestBodyLBS $ renderLBS def $ Document (Prologue [] Nothing []) root []
  httpXML $ post {requestBody = body}
  where
    xmlContent = [xml| <operation> #{opText operation} <object> #{object} <contentType> CSV |]
    root = Element "jobInfo" (M.fromList [("xmlns", "http://www.force.com/2009/06/asyncapi/dataload")]) xmlContent

closeJob :: (RestIO m, SFContext m) => BSU.ByteString -> m Document
closeJob jobid = do
  post <- bulkPost $ mappend "/services/async/v26.0/job/" jobid
  let body = RequestBodyLBS $ renderLBS def $ Document (Prologue [] Nothing []) root []
  httpXML $ post {requestBody = body}
  where
    xmlContent = [xml| <state> Closed |]
    root = Element "jobInfo" (M.fromList [("xmlns", "http://www.force.com/2009/06/asyncapi/dataload")]) xmlContent

submit :: (RestIO m, SFContext m) => BSU.ByteString -> RequestBody m -> m (V.Vector JobResult)
submit jobid csv = do
  (mgr, _) <- ask
  poster <- bulkPost $ mconcat ["/services/async/v26.0/", jobid, "/batch"]
  Response _ _ _ bs <- httpLbs poster{requestBody = csv} mgr
  case decode True bs of
    Left msg -> fail msg
    Right rows -> return $ rows

bulk :: (RestIO m, SFContext m, ToRecord a) => Operation -> Object -> V.Vector a -> m (V.Vector JobResult)
bulk operation object objs = do
  jobxml <- registerJob operation object
  let jobid = encodeUtf8 $ head $ fromDocument jobxml $.// laxElement "jobid" >=> content
      submitter = submit jobid
  liftM V.concat $ mapM (submitter . RequestBodyLBS . encode) $ splitEvery batchSize objs
  where
    batchSize = 10000
    splitEvery n v = takeWhile (not . V.null) $ V.take n v : (splitEvery n $ V.drop n v)
