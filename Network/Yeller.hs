{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.Yeller where
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified GHC.Stack
import qualified Control.Exception
import qualified Data.Typeable
import qualified Data.Aeson as JSON
import qualified Network.BSD
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Control.Concurrent.STM as STM
import Control.Applicative((<$>))
import Data.Monoid((<>))

data ErrorNotification a = ErrorNotification {
    errorType :: T.Text
  , errorMessage :: T.Text
  , errorStackTrace :: [StackFrame]
  , errorHost :: T.Text
  , errorEnvironment :: T.Text
  , errorClientVersion :: T.Text
  , errorExtra :: ExtraErrorInfo a
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

data ExtraErrorInfo a = ExtraErrorInfo {
    errorURL :: Maybe T.Text
  , errorCustomData :: Maybe (M.Map T.Text a)
  , errorLocation :: Maybe T.Text
} deriving (Show, Eq)

instance JSON.ToJSON StackOptions where
  toJSON s = JSON.object ["in-app" JSON..= stackOptionsInApp s]

instance JSON.ToJSON StackFrame where
  toJSON s = JSON.toJSON (stackFilename s, stackLineNumber s, stackFunction s, stackOptions s)

instance (JSON.ToJSON b) => JSON.ToJSON (ErrorNotification b) where
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

data Backend = Backend T.Text

data YellerClient = YellerClient {
    clientToken :: T.Text
  , clientHost :: T.Text
  , clientEnvironment :: T.Text
  , clientVersion :: T.Text
  , clientApplicationPackage :: T.Text
  , clientManager :: HTTP.Manager
  , clientBackends :: STM.TVar [Backend]
} | DisabledYellerClient

defaultBackends :: [T.Text]
defaultBackends = [
    "https://collector1.yellerapp.com"
  , "https://collector2.yellerapp.com"
  , "https://collector3.yellerapp.com"
  , "https://collector4.yellerapp.com"
  , "https://collector5.yellerapp.com"
  ]

class ToError a where
  toError :: a -> ExtraErrorInfo b -> YellerClient -> [StackFrame] -> ErrorNotification b

instance ToError Control.Exception.SomeException where
  toError e extra c stack = ErrorNotification {
        errorType = T.pack . show $ Data.Typeable.typeOf e
      , errorMessage = T.pack $ show e
      , errorStackTrace = stack
      , errorHost = clientHost c
      , errorEnvironment = clientEnvironment c
      , errorClientVersion = clientVersion c
      , errorExtra = extra
    }

parseStackLine :: String -> StackFrame
parseStackLine x = StackFrame {
    stackFilename = filename
  , stackLineNumber = line
  , stackFunction = function
  , stackOptions = StackOptions { stackOptionsInApp = True }
  }
  where encoded = T.pack x
        function = T.takeWhile (/= ' ') encoded
        afterFunction = T.tail $ T.dropWhile (/= ' ') encoded
        filename = manageNoLocationFilename $ T.dropWhile (== '(') $ T.takeWhile (/= ':') afterFunction
        afterFilename = T.dropWhile (== ':') $ T.dropWhile (/= ':') afterFunction
        line = T.take (T.length afterFilename - 1) afterFilename

manageNoLocationFilename :: T.Text -> T.Text
manageNoLocationFilename t
  | t == "<no location info>)" = ""
  | otherwise = t

parseStackTrace :: [String] -> [StackFrame]
parseStackTrace = filter ((/= "Network.Yeller.sendError") . stackFunction) . map parseStackLine

markInApp :: T.Text -> StackFrame -> StackFrame
markInApp package frame
  | T.isPrefixOf package (stackFunction frame) = frame { stackOptions = StackOptions {stackOptionsInApp = True } }
  | otherwise = frame { stackOptions = StackOptions {stackOptionsInApp = False } }

filterInAppLines :: T.Text -> [StackFrame] -> [StackFrame]
filterInAppLines package = map (markInApp package)

sendError :: (ToError e, JSON.ToJSON a) => YellerClient -> e -> ExtraErrorInfo a -> IO ()
sendError DisabledYellerClient _ _ = return ()
sendError c e extra = do
  let forced = seq e e
  stack <- GHC.Stack.whoCreated forced
  sendNotification c $ toError e extra c (filterInAppLines (clientApplicationPackage c) $ parseStackTrace stack)

sendNotification :: JSON.ToJSON a => YellerClient -> ErrorNotification a -> IO ()
sendNotification c n = sendNotificationWithRetry 0 c (JSON.encode n)

sendNotificationWithRetry :: Int -> YellerClient -> LBS.ByteString -> IO ()
sendNotificationWithRetry currentRetryCount c encoded = do
  currentBackend <- cycleBackends c
  r <- makeRequest c currentBackend encoded
  res <- Control.Exception.try $ HTTP.withResponse r (clientManager c) (\_  -> return ())
  case res of
    (Left (err :: Control.Exception.SomeException)) -> if currentRetryCount > 5 then
                                                        sendNotificationWithRetry (currentRetryCount + 1) c encoded
                                                        else
                                                          print err
    (Right _) -> return ()

cycleBackends :: YellerClient -> IO Backend
cycleBackends c = STM.atomically $ do
  modifyTVar_ (clientBackends c) cycleBackends_
  head <$> STM.readTVar (clientBackends c)

cycleBackends_ :: [Backend] -> [Backend]
cycleBackends_ (x:xs) = xs ++ [x]
cycleBackends_ xs = xs

modifyTVar_ :: STM.TVar a -> (a -> a) -> STM.STM ()
modifyTVar_ tv f = STM.readTVar tv >>= STM.writeTVar tv . f

makeRequest :: YellerClient -> Backend -> LBS.ByteString -> IO HTTP.Request
makeRequest c (Backend b) n = do
  initReq <- HTTP.parseUrl $ T.unpack (b <> clientToken c)
  let req = initReq {
      HTTP.method = "POST"
    , HTTP.secure = True
    , HTTP.requestBody = HTTP.RequestBodyLBS n
    , HTTP.redirectCount = 0
    , HTTP.requestHeaders = [("User-Agent", TE.encodeUtf8 $ clientVersion c)]
    --, HTTP.checkStatus = \_ _ _ -> Nothing
  }
  return req

data ApplicationEnvironment = TestEnvironment | ApplicationEnvironment T.Text
newtype ApplicationPackage = ApplicationPackage T.Text
newtype YellerToken = YellerToken T.Text

client :: ApplicationEnvironment -> ApplicationPackage -> YellerToken -> IO YellerClient
client TestEnvironment _ _ = return DisabledYellerClient
client (ApplicationEnvironment env) (ApplicationPackage package) (YellerToken token) = do
  h <- fmap T.pack Network.BSD.getHostName
  m <- HTTP.newManager TLS.tlsManagerSettings
  backends <- STM.newTVarIO (map Backend defaultBackends)
  return YellerClient {
        clientToken = token
      , clientHost = h
      , clientEnvironment = env
      , clientVersion = yellerVersion
      , clientApplicationPackage = package
      , clientManager = m
      , clientBackends = backends
    }
