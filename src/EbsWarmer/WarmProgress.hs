{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module EbsWarmer.WarmProgress
    ( WarmProgress(..)
    , initProgress
    , updateProgress
    ) where

import Data.Time (UTCTime)

import EbsWarmer.Units

data WarmProgress = WarmProgress
    { pStart :: UTCTime
    , pTotal :: Bytes
    , pRead :: Bytes
    , pLeft :: Bytes
    , pElapsed :: Seconds
    , pRate :: Maybe BytesPerSecond
    , pRemaining :: Maybe Seconds
    }

initProgress :: UTCTime -> Bytes -> WarmProgress
initProgress start total = WarmProgress
    { pStart = start
    , pTotal = total
    , pRead = bytes 0
    , pLeft = total
    , pElapsed = seconds 0
    , pRate = Nothing
    , pRemaining = Nothing
    }

updateProgress :: WarmProgress -> UTCTime -> Bytes -> WarmProgress
updateProgress WarmProgress{..} now bs =
    let left = diffBytes pTotal bs
        elapsed = diffSeconds now pStart
        rate = bytesPerSecond bs elapsed

    in WarmProgress
        { pStart = pStart
        , pTotal = pTotal
        , pRead = bs
        , pLeft = left
        , pElapsed = elapsed
        , pRate = Just rate
        , pRemaining = Just $ secondsRemaining left rate
        }
