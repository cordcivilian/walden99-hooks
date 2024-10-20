module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

hooks :: Wai.Application
hooks request respond = do
    body <- Wai.lazyRequestBody request
    respond $ case (request_method, request_path) of
        ("GET", "/") -> rootRoute request
        ("POST", "/test") -> testRoute request
        ("POST", "/hephaestion") -> hookRoute request body
        _ -> notFoundRoute
        where
            request_method = BS.unpack $ Wai.requestMethod request
            request_path = BS.unpack $ Wai.rawPathInfo request

loggedHooks :: Wai.Application
loggedHooks = Mid.logStdout hooks

rootRoute :: Wai.Request -> Wai.Response
rootRoute request = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (
        BSL.concat [
            BSL.fromStrict $ Wai.requestMethod request,
            BSL.pack "\n",
            BSL.pack "Github Webhook Base"
                   ]
    )

testRoute :: Wai.Request -> Wai.Response
testRoute request = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack $ show $ Wai.requestBodyLength request)

hookRoute :: Wai.Request -> BSL.ByteString -> Wai.Response
hookRoute request body = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (
        BSL.concat [
            body,
            BSL.pack "\n",
            BSL.pack $ show $ Wai.queryString request,
            BSL.pack "\n",
            BSL.pack $ show $ Wai.requestHeaders request
                   ]
    )

notFoundRoute :: Wai.Response
notFoundRoute = Wai.responseLBS
    HTTP.status404
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack "404 - Not Found")

main :: IO ()
main = do
    let port = 8888
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    Warp.run port loggedHooks
