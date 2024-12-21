{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (lookupEnv)

import Data.Bits (xor, (.|.))
import Data.Word (Word8)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Digest.Pure.SHA as SHA
import qualified Data.Aeson as JSON

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified Data.Text.Encoding as TE

type Logger = T.Text -> IO ()

hooks :: Logger -> Wai.Application
hooks logger request respond = do
    body <- Wai.lazyRequestBody request
    case (request_method, request_path) of
        ("GET", "/") -> do
            response <- rootRoute logger request
            respond response
        ("POST", "/test") -> do
            response <- testRoute logger request
            respond response
        ("POST", "/hook") -> do
            response <- hookRoute logger request body
            respond response
        _ -> do
            response <- notFoundRoute logger
            respond response
        where
            request_method = BS.unpack $ Wai.requestMethod request
            request_path = BS.unpack $ Wai.rawPathInfo request

loggedHooks :: Wai.Application
loggedHooks = Mid.logStdout $ hooks TIO.putStrLn

rootRoute :: Logger -> Wai.Request -> IO Wai.Response
rootRoute logger request = do 
    logger $ T.pack "123"
    return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/plain")]
        (
            BSL.concat [
                BSL.fromStrict $ Wai.requestMethod request,
                BSL.pack "\n",
                BSL.pack "Github Webhook Base"
                       ]
        )

testRoute :: Logger -> Wai.Request -> IO Wai.Response
testRoute logger request = do
    logger $ T.pack "test345"
    return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/plain")]
        (BSL.pack $ show $ Wai.requestBodyLength request)

hookRoute :: Logger -> Wai.Request -> BSL.ByteString -> IO Wai.Response
hookRoute logger request body = do
    maybeSecret <- lookupEnv "HOOKER"
    let secret = maybe "" id maybeSecret
        signature = lookup "x-hub-signature-256" $ Wai.requestHeaders request
    case (verifySignature body signature secret, getEventInfo body) of
      (Right (), Just event) -> logger $ T.pack $ show event
      (_, _) -> logger $ T.pack "invalid event"
    return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/plain")]
        (BSL.pack "event processed")

getEventInfo :: BSL.ByteString -> Maybe Event
getEventInfo body = case JSON.eitherDecode body of
                      Right event -> Just event
                      _ -> Nothing

notFoundRoute :: Logger -> IO Wai.Response
notFoundRoute _ = return $ Wai.responseLBS
    HTTP.status404
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack "404 - Not Found")

data Event = Event { ref :: String , repoName :: String } deriving (Show)

instance JSON.FromJSON Event where
  parseJSON = JSON.withObject "Event" $ \v -> Event
    <$> v JSON..: "ref"
    <*> (v JSON..: "repository" >>= (JSON..: "name"))

verifySignature :: BSL.ByteString
                -> Maybe BS.ByteString
                -> String
                -> Either T.Text ()
verifySignature body signature secret = do
    case signature of
        Nothing -> Left "missing signature headers"
        Just digest -> do
            let
                packedSecret = BSL.pack secret
                hmacInstance = SHA.hmacSha256 packedSecret body
                expected = BS.pack $ SHA.showDigest $ hmacInstance
                actual = TE.encodeUtf8 $ T.drop 7 $ TE.decodeUtf8 digest
            if constantTimeCompare expected actual
                then Right ()
                else Left "signatures do not match"

constantTimeCompare :: BS.ByteString -> BS.ByteString -> Bool
constantTimeCompare a b =
    BS.length a == BS.length b &&
        0 == foldl' (\acc (x, y) -> acc .|. xor x y) (0 :: Word8) (B.zip a b)

-- curl -v 'http://127.0.0.1:8888/hook?123=123' -d testing=123
main :: IO ()
main = do
    let port = 8888
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    Warp.run port loggedHooks
