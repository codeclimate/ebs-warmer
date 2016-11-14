module EbsWarmer.WarmProgressSpec
    ( main
    , spec
    ) where

import Test.Hspec

import Control.Monad.IO.Class (liftIO)
import Data.Time (addUTCTime, getCurrentTime)

import EbsWarmer.WarmProgress
import EbsWarmer.Units

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "WarmProgress" $ do
    describe "initProgress" $ do
        it "creates an intialized progress value" $ do
            now <- liftIO $ getCurrentTime

            let total = bytes 1234
                progress = initProgress now total

            pStart progress `shouldBe` now
            pTotal progress `shouldBe` total
            pRead progress `shouldBe` bytes 0
            pLeft progress `shouldBe` total
            pElapsed progress `shouldBe` seconds 0
            pRead progress `shouldBe` bytes 0
            pRemaining progress `shouldBe` Nothing

    describe "updateProgress" $ do
        it "updates read, left, elapsed, rate, and remaining" $ do
            now <- liftIO $ getCurrentTime

            let total = bytes 1000
                p = initProgress now $ total
                p1 = updateProgress p (addUTCTime 10 now) $ bytes 250
                p2 = updateProgress p1 (addUTCTime 30 now) $ bytes 600

            pRead p1 `shouldBe` bytes 250
            pLeft p1 `shouldBe` bytes 750
            pElapsed p1 `shouldBe` seconds 10
            pRate p1 `shouldBe` Just (bytesPerSecond (bytes 25) (seconds 1))
            pRemaining p1 `shouldBe` Just (seconds 30)

            pRead p2 `shouldBe` bytes 600
            pLeft p2 `shouldBe` bytes 400
            pElapsed p2 `shouldBe` seconds 30
            pRate p2 `shouldBe` Just (bytesPerSecond (bytes 20) (seconds 1))
            pRemaining p2 `shouldBe` Just (seconds 20)
