{-# LANGUAGE OverloadedStrings #-}
module Network.Yeller where
import qualified Data.Map as M
import qualified Data.Text as T
import qualified GHC.Stack
import qualified Control.Exception
import qualified Data.Typeable
import qualified Data.Aeson as JSON
import qualified Network.BSD
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS

data ErrorNotification = ErrorNotification {
    errorType :: T.Text
  , errorMessage :: T.Text
  , errorStackTrace :: [StackFrame]
  , errorHost :: T.Text
  , errorEnvironment :: T.Text
  , errorClientVersion :: T.Text
  , errorExtra :: ExtraErrorInfo
} deriving (Show, Eq)

data StackFrame = StackFrame {
    stackFilename :: T.Text
  , stackLineNumber :: T.Text
  , stackFunction :: T.Text
  , stackOptions :: StackOptions
} deriving (Show, Eq)

data StackOptions = StackOptions {
  stackOptionsInApp :: Bool
} deriving (Show, Eq)

data ExtraErrorInfo = ExtraErrorInfo {
    errorURL :: Maybe T.Text
  , errorCustomData :: Maybe (M.Map T.Text JSON.Value)
  , errorLocation :: Maybe T.Text
} deriving (Show, Eq)

instance JSON.ToJSON StackOptions where
  toJSON s = JSON.object ["in-app" JSON..= stackOptionsInApp s]

instance JSON.ToJSON StackFrame where
  toJSON s = JSON.toJSON (stackFilename s, stackLineNumber s, stackFunction s, stackOptions s)

instance JSON.ToJSON ErrorNotification where
  toJSON e = JSON.object ["type" JSON..= errorType e
                          , "message" JSON..= errorMessage e
                          , "stacktrace" JSON..= errorStackTrace e
                          , "host" JSON..= errorHost e
                          , "application-environment" JSON..= errorEnvironment e
                          , "client-version" JSON..= errorClientVersion e
                          , "url" JSON..= errorURL ( errorExtra e)
                          , "location" JSON..= errorLocation ( errorExtra e)
                          , "custom-data" JSON..= errorCustomData ( errorExtra e)
                          ]

yellerVersion :: T.Text
yellerVersion = T.pack "yeller-haskell: 0.0.1"

data YellerClient = YellerClient {
    clientHost :: T.Text
  , clientEnvironment :: T.Text
  , clientVersion :: T.Text
} deriving (Show, Eq)

class ToError a where
  toError :: a -> ExtraErrorInfo -> YellerClient -> ErrorNotification

instance ToError Control.Exception.SomeException where
  toError e extra c = ErrorNotification {
        errorType = T.pack . show $ Data.Typeable.typeOf e
      , errorMessage = T.pack $ show e
      , errorStackTrace = parseStackTrace (GHC.Stack.whoCreated e)
      , errorHost = clientHost c
      , errorEnvironment = clientEnvironment c
      , errorClientVersion = clientVersion c
      , errorExtra = extra
    }

parseStackTrace :: a -> [StackFrame]
parseStackTrace _ = []

sendError :: ToError e => YellerClient -> e -> ExtraErrorInfo -> IO ()
sendError c e extra = sendNotification c $ toError e extra c

sendNotification :: YellerClient -> ErrorNotification -> IO ()
sendNotification c n = do
  r <- makeRequest c n
  m <- HTTP.newManager TLS.tlsManagerSettings
  _ <- HTTP.withResponse r m (\_  -> return ())
  return ()

makeRequest :: YellerClient -> ErrorNotification -> IO HTTP.Request
makeRequest _ n = do
  initReq <- HTTP.parseUrl "https://collector1.yellerapp.com/API_KEY_HERE"
  let req = initReq {
      HTTP.method = "POST"
    , HTTP.secure = True
    , HTTP.requestBody = HTTP.RequestBodyLBS (JSON.encode n)
    , HTTP.redirectCount = 0
    --, HTTP.checkStatus = \_ _ _ -> Nothing
  }
  return req

newtype ApplicationEnvironment = ApplicationEnvironment T.Text

client :: ApplicationEnvironment -> IO YellerClient
client (ApplicationEnvironment env) = do
  h <- fmap T.pack Network.BSD.getHostName
  return YellerClient {
      clientHost = h
      , clientEnvironment = env
      , clientVersion = yellerVersion
    }

main :: IO ()
main = do
  c <- client (ApplicationEnvironment "production")
  sendError c (Control.Exception.SomeException Control.Exception.DivideByZero) (ExtraErrorInfo Nothing Nothing Nothing)
