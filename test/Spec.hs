{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Text
import ByteString

main :: IO ()
main = hspec $ do
    describe "text" Text.spec
    describe "bytestring" ByteString.spec
