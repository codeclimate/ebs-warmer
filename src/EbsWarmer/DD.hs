{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EbsWarmer.DD
    ( DDLine(..)
    , spawnDD
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import System.Exit (ExitCode)
import System.IO (hGetLine)
import System.Process
    ( StdStream(CreatePipe)
    , createProcess
    , proc
    , spawnCommand
    , std_err
    , waitForProcess
    )

import qualified Control.Exception as E

import EbsWarmer.Units (Bytes, bytes)

data DDLine
    = RecordsIn String
    | RecordsOut String
    | Progress Bytes
    | Unexpected String

spawnDD :: Int -> [String] -> (DDLine -> IO a) -> IO ExitCode
spawnDD delay args f = do
    (_, _, Just h, p) <- createProcess $
        (proc "dd" args) { std_err = CreatePipe }

    -- Start a thread reading stderr and invoking the callback
    void $ forkIO $ forever $ do
        ln <- hGetLine h `E.catch` ignoreIOException ""
        f $ parseDDLine ln

    -- Start a thread reporting progress every interval microseconds
    void $ forkIO $ forever $ do
        threadDelay delay
        void $ spawnCommand "killall --signal USR1 dd 2>/dev/null 1>&2"

    -- Wait for DD and return its exit code
    waitForProcess p

  where
    ignoreIOException :: a -> E.IOException -> IO a
    ignoreIOException x _ = return x

parseDDLine :: String -> DDLine
parseDDLine ln = case words ln of
    (r:"records":"in":[]) -> RecordsIn r
    (r:"records":"out":[]) -> RecordsOut r
    (n:"bytes":_) -> Progress $ bytes $ read n
    _ -> Unexpected ln
