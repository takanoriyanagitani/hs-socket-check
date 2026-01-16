module Main (main) where

import SocketCheck (TimeoutMs (..), run)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | Command line options
data Options = Options
    { optTimeout :: TimeoutMs
    , optSocketPath :: Maybe FilePath
    , optShowHelp :: Bool
    }

-- | Default options
defaultOptions :: Options
defaultOptions =
    Options
        { optTimeout = TimeoutMs 1000
        , optSocketPath = Nothing
        , optShowHelp = False
        }

header :: String
header = "Usage: hs-socket-check [OPTION...]"

-- | Command line options description
options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['t']
        ["timeout"]
        (ReqArg (\arg opt -> opt{optTimeout = TimeoutMs (read arg)}) "MILLISECONDS")
        "Timeout in milliseconds (default: 1000)"
    , Option
        ['s']
        ["socket"]
        (ReqArg (\arg opt -> opt{optSocketPath = Just arg}) "PATH")
        "Path to the Unix domain socket (required)"
    , Option
        ['h']
        ["help"]
        (NoArg (\opt -> opt{optShowHelp = True}))
        "Show this help message"
    ]

-- | Pure parser logic
parse :: [String] -> Either String Options
parse argv =
    case getOpt Permute options argv of
        (o, [], []) -> Right $ foldl (flip id) defaultOptions o
        (_, _, errs) -> Left (concat errs ++ usageInfo header options)

-- | Handle parsed options
handleOpts :: TimeoutMs -> Maybe FilePath -> Bool -> IO ()
handleOpts _ _ True = do
    putStrLn (usageInfo header options)
    exitSuccess
handleOpts _ Nothing False = do
    hPutStrLn stderr ("Socket path is required.\n" ++ usageInfo header options)
    exitFailure
handleOpts timeout (Just path) False = run timeout path

-- | Handle result of parsing arguments
handleResult :: Either String Options -> IO ()
handleResult (Left err) = do
    hPutStrLn stderr err
    exitFailure
handleResult (Right opts) =
    handleOpts (optTimeout opts) (optSocketPath opts) (optShowHelp opts)

main :: IO ()
main = do
    args <- getArgs
    handleResult (parse args)
