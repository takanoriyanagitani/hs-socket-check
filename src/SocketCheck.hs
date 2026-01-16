module SocketCheck (
    tryCreate,
    tryConnect,
    TimeoutMs (..),
    path2addr,
    SocketStatus (..),
    tryCheckSocket,
    ioe2status,
    status2ltsv,
    msg2ltsv,
    checkSocket,
    run,
) where

import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Timeout (timeout)

import qualified Network.Socket as S

import Control.Exception (IOException, try)

data SocketStatus
    = Ok
    | Closed
    | Timeout
    | NotFound
    | PermissionDenied
    | ResourceExhausted
    | CreateError String
    | Others String

status2ltsv :: SocketStatus -> String
status2ltsv Ok = "status:accepted"
status2ltsv Closed = "status:closed"
status2ltsv Timeout = "status:timeout"
status2ltsv NotFound = "status:missing"
status2ltsv PermissionDenied = "status:denied"
status2ltsv ResourceExhausted = "status:tooMany"
status2ltsv (CreateError _) = "status:nosock"
status2ltsv (Others _) = "status:other"

msg2ltsv :: FilePath -> SocketStatus -> String
msg2ltsv path Ok =
    "level:info"
        ++ "\t"
        ++ "status:accepted"
        ++ "\t"
        ++ "path:"
        ++ path
msg2ltsv path (CreateError _) =
    "level:fatal"
        ++ "\t"
        ++ "status:nosock"
        ++ "\t"
        ++ "path:"
        ++ path
msg2ltsv path status =
    "level:error"
        ++ "\t"
        ++ status2ltsv status
        ++ "\t"
        ++ "path:"
        ++ path

tryCreate :: IO (Either IOException S.Socket)
tryCreate = try (S.socket S.AF_UNIX S.Stream S.defaultProtocol)

tryConnect :: S.Socket -> S.SockAddr -> IO (Either IOException ())
tryConnect sock addr = try (S.connect sock addr)

path2addr :: FilePath -> S.SockAddr
path2addr = S.SockAddrUnix

newtype TimeoutMs = TimeoutMs Int

isResourceExhausted :: IOException -> Bool
isResourceExhausted ioe = "resource exhausted" == show ioe

ioe2status :: IOException -> SocketStatus
ioe2status ioe
    | isDoesNotExistError ioe = NotFound
    | isPermissionError ioe = PermissionDenied
    | isResourceExhausted ioe = ResourceExhausted
    | otherwise = Others (show ioe)

handleResult :: Maybe (Either IOException ()) -> SocketStatus
handleResult (Just (Right ())) = Ok
handleResult (Just (Left ioe)) = ioe2status ioe
handleResult Nothing = Timeout

tryCheckSocket :: TimeoutMs -> Either IOException S.Socket -> S.SockAddr -> IO SocketStatus
tryCheckSocket (TimeoutMs wait) (Right sock) addr = do
    let timeOutMicro :: Int = wait * 1000
    connResult <- timeout timeOutMicro (tryConnect sock addr)
    S.close sock -- will throw on close error(fatal error)
    pure $ handleResult connResult
tryCheckSocket (TimeoutMs _) (Left _) _ = do
    pure (CreateError "unable to create socket")

checkSocket :: TimeoutMs -> FilePath -> IO SocketStatus
checkSocket wait path = do
    let addr :: S.SockAddr = path2addr path
    esock :: Either IOException S.Socket <- tryCreate
    tryCheckSocket wait esock addr

run :: TimeoutMs -> FilePath -> IO ()
run wait path = do
    status :: SocketStatus <- checkSocket wait path
    let ltsv :: String = msg2ltsv path status
    putStrLn ltsv
