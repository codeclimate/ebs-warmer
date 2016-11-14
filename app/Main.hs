{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid ((<>))
import Data.Time (getCurrentTime)
import Options.Applicative
import System.Exit (exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess)

import EbsWarmer

data Options = Options
    { oInterval :: Seconds
    , oDevice :: String
    }

options :: Parser Options
options = Options
    <$> (seconds <$> option auto
        (  short 'i'
        <> long "interval"
        <> metavar "SECONDS"
        <> help "Interval on which to print progress information"
        <> value 3
        ))
    <*> argument str
        (  metavar "DEVICE"
        <> help "Block device to read, e.g. /dev/sda1"
        )

main :: IO ()
main = do
    opts <- parseOptions
    progress <- initProgress
        <$> getCurrentTime
        <*> getBlockSize (oDevice opts)

    let delay = toMicroseconds $ oInterval opts
        args = ["if=" <> oDevice opts, "of=/dev/null"]

    exitWith =<< spawnDD delay args (handleDDLine progress)

handleDDLine :: WarmProgress -> DDLine -> IO ()
handleDDLine p (Progress bs) = do
    now <- getCurrentTime

    let p' = updateProgress p now bs

    putStrLn $ mconcat
        [ "bytes=", show $ pRead p'
        , " total=", show $ pTotal p'
        , " rate=", maybe "" show $ pRate p'
        , " remaining=", maybe "" show $ pRemaining p'
        ]

handleDDLine _ (Unexpected x) = hPutStrLn stderr $ "dd stderr: " <> x
handleDDLine _ _ = return ()

parseOptions :: IO Options
parseOptions = execParser $ info (helper <*> options)
    (  fullDesc
    <> progDesc "Warm an attached EBS volume by reading all its bytes"
    )

getBlockSize :: String -> IO Bytes
getBlockSize d = bytes . read <$> readProcess "blockdev" ["--getsize64", d] ""
