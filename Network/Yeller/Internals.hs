{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.Yeller.Internals where
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified GHC.Stack
import qualified Control.Exception
import qualified Data.Typeable
import qualified Data.Aeson as JSON
import qualified Network.BSD
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPStatus
import qualified Network.HTTP.Client.TLS as TLS
import System.IO as IO
import qualified Control.Concurrent.STM as STM
import Control.Applicative((<$>))
import Data.Monoid((<>))

-- | An error notification. This is what is sent to Yeller's servers
data ErrorNotification a = ErrorNotification {
    -- | The type of the error. E.g. SomeException, DivideByZero
    errorType :: T.Text

    -- | The message of the exception
  , errorMessage :: T.Text

    -- | The stacktrace of the error. Usually grabbed using 'GHC.Stack.whoCreated'
  , errorStackTrace :: [StackFrame]

    -- | the host that this error occurred on
  , errorHost :: T.Text

    -- | the application environment this error occurred in
  , errorEnvironment :: T.Text

    -- | the version of the yeller client reporting the error
  , errorClientVersion :: T.Text

    -- | see 'ExtraErrorInfo'
  , errorExtra :: ExtraErrorInfo a
} deriving (Show, Eq)

-- | A line of the stacktrace
data StackFrame = StackFrame {
    -- | The filename this line occurred in
    stackFilename :: T.Text

    -- | The line number(s) this line occurred in
  , stackLineNumber :: T.Text

    -- | the function this line occurred in
  , stackFunction :: T.Text

    -- see 'StackOptions'
  , stackOptions :: StackOptions
} deriving (Show, Eq)

-- | Options to be associated with each
-- | line in the stacktrace. Currently
-- | only supports if the line is in
-- | the application or not.
data StackOptions = StackOptions {
  stackOptionsInApp :: Bool
} deriving (Show, Eq)

-- | Extra error information to be passed along with
-- | an error. All fields are optional.
data ExtraErrorInfo a = ExtraErrorInfo {
    -- | If the error happened during a web request,
    -- | the url that was hit during that request
    errorURL :: Maybe T.Text
    -- | A map of data that has to conform to Aeson's
    -- | ToJSON typeclass. Can be anything
    -- | you want that helps you debug the error.
  , errorCustomData :: Maybe (M.Map T.Text a)

    -- | what toplevel part of the application the
    -- | request that caused the error came from
    -- | e.g. web controller name, background job name
  , errorLocation :: Maybe T.Text

    -- | which user the error happened from.
    -- | Lets you see how many total users were affected by an error
  , errorUser :: Maybe UserInfo

    -- | which http request was happening when
    -- | the error occurred
  , errorHTTPRequest :: Maybe HTTPRequest
} deriving (Show, Eq)

-- | lets you attach which user the error happened with
-- | which lets you count the total number of affected users
data UserInfo = UserInfo {
  -- | the user id of the affected user
  userID :: Integer
} deriving (Show, Eq)

instance JSON.ToJSON UserInfo where
  toJSON u = JSON.object ["id" JSON..= userID u]

-- | lets you attach which http request was occurring when the
-- | error happened
-- | which lets you see the browser, if the request came from a spider
-- | and so on
data HTTPRequest = HTTPRequest {
  -- | the user agent of the impacted http request
  httpRequestUserAgent :: T.Text
} deriving (Show, Eq)

instance JSON.ToJSON HTTPRequest where
  toJSON h = JSON.object ["user-agent" JSON..= httpRequestUserAgent h]

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
                          , "custom-data" JSON..= makeJSONCustomData ( errorExtra e)
                          ]

makeJSONCustomData :: JSON.ToJSON a => ExtraErrorInfo a -> JSON.Value
makeJSONCustomData e = JSON.toJSON (maybeAddHTTPUserAgent e (maybeAddUser e withJSONVals))
  where withJSONVals = M.map JSON.toJSON (maybeOr (errorCustomData e) M.empty)

maybeAddUser :: ExtraErrorInfo a -> M.Map T.Text JSON.Value -> M.Map T.Text JSON.Value
maybeAddUser (ExtraErrorInfo { errorUser =  Nothing } ) m = m
maybeAddUser (ExtraErrorInfo { errorUser = (Just user) }) m =
  case M.lookup "user" m of
    Nothing -> M.insert "user" (JSON.toJSON user) m
    (Just (JSON.Object u)) -> M.insert "user" (JSON.Object (HM.insert "id" (JSON.toJSON (userID user)) u)) m
    (Just _)  -> m

maybeAddHTTPUserAgent :: ExtraErrorInfo a -> M.Map T.Text JSON.Value -> M.Map T.Text JSON.Value
maybeAddHTTPUserAgent (ExtraErrorInfo { errorHTTPRequest =  Nothing } ) m = m
maybeAddHTTPUserAgent (ExtraErrorInfo { errorHTTPRequest = (Just http) }) m =
  case M.lookup "http-request" m of
    Nothing -> M.insert "http-request" (JSON.toJSON http) m
    (Just (JSON.Object h)) -> M.insert "http-request" (JSON.Object (HM.insert "user-agent" (JSON.toJSON (httpRequestUserAgent http)) h)) m
    (Just _)  -> m

maybeOr :: Maybe a -> a -> a
maybeOr (Just a) _ = a
maybeOr Nothing a = a

yellerVersion :: T.Text
yellerVersion = T.pack "yeller-haskell: 0.1.0.1"

newtype Backend = Backend T.Text

-- | A Yeller client.
-- | Build one with 'client' like so:
-- | client (defaultClientSettings { clientSettingsToken = YellerToken "YOUR_TOKEN_HERE" })
-- |
-- |
-- Used to keep persistent
-- | http connections alive, keep track of
-- | which endpoint to send to, and a bunch of
-- | data that's sent along with the error.
data YellerClient = YellerClient {
    -- | The token used to authenticate with Yeller's servers.
    clientToken :: T.Text

    -- | The server this client is running on.
  , clientHost :: T.Text

    -- | The environment this client is running in
  , clientEnvironment :: T.Text

    -- | The version number of this client
  , clientVersion :: T.Text
    -- | The name of the application package that is using this client
  , clientApplicationPackage :: T.Text

    -- | The http manager for tracking open connections
  , clientManager :: HTTP.Manager

    -- | the set of backends to use. The next backend that will
    -- | be used is second in the list.
  , clientBackends :: STM.TVar [Backend]
    -- | the client error handler. See 'YellerClientErrorHandler'
  , clientErrorHandler :: YellerClientErrorHandler
    -- | the maximum number of retries
  , clientMaxRetries :: Int
} | DisabledYellerClient

-- | An error handler, for dealing with errors when sending errors to Yeller's servers
-- | 'defaultErrorHandler' just prints to stderr when receiving errors, but you
-- | might want to override that to make it go to your logging system of choice.
data YellerClientErrorHandler = YellerClientErrorHandler {
    -- | used when handling authentication (401, 403) failures from Yeller's servers.
    -- | These errors are *not* retried like other ones.
    handleAuthenticationErrors :: HTTP.Response HTTP.BodyReader -> IO ()
    -- | used when handling any errors that aren't authentication related.
    -- | This function will only be called after 'clientMaxRetries' is
    -- | exceeded.
  , handleIOErrors :: Control.Exception.SomeException -> ErrorNotification JSON.Value -> IO ()
}

-- | the Environment your client is running in. If set to 'TestEnvironment',
-- | then no errors will be reported (which you should do for development/testing)
data ApplicationEnvironment = TestEnvironment | ApplicationEnvironment T.Text

-- | The name of the package your application is in.
newtype ApplicationPackage = ApplicationPackage T.Text

-- | An api token. Get one from your project's settings page
newtype YellerToken = YellerToken T.Text

-- | Options you pass when creating a client, usually done like this:
-- | client (defaultClientSettings { clientSettingsToken = YellerToken "YOUR_TOKEN_HERE" })
data YellerClientSettings = YellerClientSettings {
    -- | The api token used for authenticating with Yeller's servers
    clientSettingsToken :: YellerToken

    -- | (optional): the name of the server this client is running on
    -- | if set to Nothing, 'client' will default it to 'Network.BSD.getHostName'
  , clientSettingsHost :: Maybe T.Text

    -- | the name of the environment the application is running in
    -- | for example: production, test etc
    -- | if set to TestEnvironment, the client will be disabled
  , clientSettingsEnvironment :: ApplicationEnvironment

    -- | the name of the application package, used to
    -- | filter out noisy stacktrace lines by default in
    -- | the UI. Lines from functions starting with this
    -- | are marked as "in-app", others will only show
    -- | up via a toggle in the UI.
  , clientSettingsApplicationPackage :: ApplicationPackage

    -- | a list of servers to contact. Weird things will
    -- | probably happen if it's set to []
  , clientSettingsBackends :: [Backend]

    -- | used for handling errors when sending to Yeller's
    -- | servers. See 'YellerClientErrorHandler' for more.
  , clientSettingsErrorHandler :: YellerClientErrorHandler

    -- | the maximum number of times to retry sending to
    -- | the servers before logging a failure
  , clientSettingsMaxRetries :: Int
}

-- | the default client settings, used for constructing a client
defaultClientSettings :: YellerClientSettings
defaultClientSettings = YellerClientSettings {
    clientSettingsToken = YellerToken bogusClientToken
  , clientSettingsHost = Nothing
  , clientSettingsEnvironment = ApplicationEnvironment "production"
  , clientSettingsApplicationPackage = ApplicationPackage ""
  , clientSettingsBackends = map Backend defaultBackends
  , clientSettingsErrorHandler = defaultErrorHandler
  , clientSettingsMaxRetries = 10
}

defaultErrorHandler :: YellerClientErrorHandler
defaultErrorHandler = YellerClientErrorHandler {
  handleAuthenticationErrors = \_ -> IO.hPutStrLn stderr "Failed to authenticate with the yeller servers. Check your api key is correct and try again."
  , handleIOErrors = \x n -> IO.hPrint stderr ("Error sending an exception to Yeller: " :: String, x, n)
}

bogusClientToken :: T.Text
bogusClientToken = "YOUR_API_TOKEN_HERE"

defaultBackends :: [T.Text]
defaultBackends = [
    "https://collector1.yellerapp.com/"
  , "https://collector2.yellerapp.com/"
  , "https://collector3.yellerapp.com/"
  , "https://collector4.yellerapp.com/"
  , "https://collector5.yellerapp.com/"
  ]

-- | A class for converting things into errors
-- | Means you can pass both exceptions, and any
-- | other error like values in your code
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

-- | Sends an error to Yeller's servers
-- | needs a client, something that can be turned into an error (via ToError), and
-- | extra error information (which describes additional information to be sent
-- | along with the error)
sendError :: (ToError e, JSON.ToJSON a) => YellerClient -> e -> ExtraErrorInfo a -> IO ()
sendError DisabledYellerClient _ _ = return ()
sendError c e extra = do
  let forced = seq e e
  stack <- GHC.Stack.whoCreated forced
  sendNotification c $ toError e extra c (filterInAppLines (clientApplicationPackage c) $ parseStackTrace stack)

sendNotification :: JSON.ToJSON a => YellerClient -> ErrorNotification a -> IO ()
sendNotification c n = sendNotificationWithRetry 0 c n (JSON.encode n)

sendNotificationWithRetry :: JSON.ToJSON a => Int -> YellerClient -> ErrorNotification a -> LBS.ByteString -> IO ()
sendNotificationWithRetry currentRetryCount c n encoded = do
  currentBackend <- cycleBackends c
  r <- makeRequest c currentBackend encoded
  res <- Control.Exception.try $ HTTP.withResponse r (clientManager c) return
  case res of
    (Left (err :: Control.Exception.SomeException)) -> if currentRetryCount > clientMaxRetries c then
                                                          sendNotificationWithRetry (currentRetryCount + 1) c n encoded
                                                        else
                                                          handleIOErrors (clientErrorHandler c) err (encodeCustomDataAsJSON n)
    (Right httpResponse) -> handleNonExceptionalSendRequest httpResponse currentRetryCount c n encoded

handleNonExceptionalSendRequest :: JSON.ToJSON a => HTTP.Response HTTP.BodyReader -> Int -> YellerClient -> ErrorNotification a -> LBS.ByteString -> IO ()
handleNonExceptionalSendRequest res currentRetryCount c n encoded
  | status == 403 || status == 401 = handleAuthenticationErrors (clientErrorHandler c) res
  | status > 299 = sendNotificationWithRetry (currentRetryCount + 1) c n encoded
  | otherwise = return ()
  where status = HTTPStatus.statusCode (HTTP.responseStatus res)

encodeCustomDataAsJSON :: JSON.ToJSON a => ErrorNotification a -> ErrorNotification JSON.Value
encodeCustomDataAsJSON n = n { errorExtra = (errorExtra n) { errorCustomData = Just (M.fromList [("data", JSON.toJSON (errorCustomData (errorExtra n)))]) } }

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
    , HTTP.checkStatus = \_ _ _ -> Nothing
  }
  return req

-- | Initializes a Yeller client, given some client settings
-- | Call it like this with the default client settings:
-- | client (defaultClientSettings { clientSettingsToken = YellerToken "YOUR_TOKEN_HERE" })
client :: YellerClientSettings -> IO YellerClient
client (YellerClientSettings {clientSettingsEnvironment=TestEnvironment}) = return DisabledYellerClient
client c@(YellerClientSettings {clientSettingsEnvironment=(ApplicationEnvironment env), clientSettingsApplicationPackage=(ApplicationPackage package), clientSettingsToken=(YellerToken token)}) = do
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
      , clientErrorHandler = clientSettingsErrorHandler c
      , clientMaxRetries = clientSettingsMaxRetries c
    }

shutdownClient :: YellerClient -> IO ()
shutdownClient c = HTTP.closeManager (clientManager c)
