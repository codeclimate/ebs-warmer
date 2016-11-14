{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module EbsWarmer.UnitsSpec
    ( main
    , spec
    ) where

import Test.Hspec

import EbsWarmer.Units

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Units" $ do
    describe "Bytes" $ do
        it "has a friendly show instance" $ do
            let b = 1024
                bs = bytes 123
                kb = bytes $ 4 * b + 500
                mb = bytes $ 5 * (b ^ 2) + 300000
                gb = bytes $ 6 * (b ^ 3) + 200000000
                tb = bytes $ 700 * (b ^ 4) + 6000000000000

            show bs `shouldBe` "123B"
            show kb `shouldBe` "4.4KB"
            show mb `shouldBe` "5.2MB"
            show gb `shouldBe` "6.1GB"
            show tb `shouldBe` "705.4TB"

    describe "Seconds" $ do
        it "has a friendly show instance" $ do
            let s = seconds $ 35
                m = seconds $ 3 * 60 + 5
                h = seconds $ 60 * 60 + 3 * 60 + 5
                d = seconds $ 60 * 60 * 24 + 760

            show s `shouldBe` "35s"
            show m `shouldBe` "3m5s"
            show h `shouldBe` "1h3m5s"
            show d `shouldBe` "1d12m40s"
