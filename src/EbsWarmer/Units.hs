module EbsWarmer.Units
    ( Bytes
    , bytes
    , diffBytes
    , Seconds
    , seconds
    , diffSeconds
    , BytesPerSecond
    , bytesPerSecond
    , secondsRemaining
    , toMicroseconds
    ) where

import Data.Monoid ((<>))
import Data.Time (UTCTime, diffUTCTime)

newtype Bytes = Bytes Integer deriving Eq

instance Show Bytes where
    show (Bytes i)
        | i > terabyte = roundTo terabyte "TB"
        | i > gigabyte = roundTo gigabyte "GB"
        | i > megabyte = roundTo megabyte "MB"
        | i > kilobyte = roundTo kilobyte "KB"
        | otherwise = show i <> "B"
      where
        roundTo u l =
            let x = fromIntegral i / fromIntegral u
            in showRounded 1 x <> l

        terabyte = 1024 * gigabyte
        gigabyte = 1024 * megabyte
        megabyte = 1024 * kilobyte
        kilobyte = 1024

newtype Seconds = Seconds Integer deriving Eq

instance Show Seconds where
    show (Seconds i)
        | i > day = split day "d"
        | i > hour = split hour "h"
        | i > minute = split minute "m"
        | otherwise = show i <> "s"
      where
        split u l =
            let (q, r) = i `divMod` u
            in show q <> l <> show (Seconds r)

        day = hour * 24
        hour = minute * 60
        minute = 60

newtype BytesPerSecond = BytesPerSecond Double deriving Eq

instance Show BytesPerSecond where
    show (BytesPerSecond d) = show (Bytes $ round d) <> "/s"

bytes :: Integer -> Bytes
bytes = Bytes

diffBytes :: Bytes -> Bytes -> Bytes
diffBytes (Bytes a) (Bytes b) = Bytes $ a - b

seconds :: Integer -> Seconds
seconds = Seconds

-- N.B. Uses truncation rounding strategy
diffSeconds :: UTCTime -> UTCTime -> Seconds
diffSeconds a b =
    Seconds $ fst $ properFraction $ diffUTCTime a b

bytesPerSecond :: Bytes -> Seconds -> BytesPerSecond
bytesPerSecond (Bytes a) (Seconds b) = BytesPerSecond
    $ fromIntegral a / fromIntegral b

secondsRemaining :: Bytes -> BytesPerSecond -> Seconds
secondsRemaining (Bytes b) (BytesPerSecond ps) =
    Seconds $ round $ fromIntegral b / ps

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds i) = fromIntegral i * 1000000

showRounded :: Int -> Double -> String
showRounded places number =
    let (x, y) = break (== '.') $ show number
    in x <> take (places + 1) y
