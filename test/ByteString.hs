{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ByteString where

import Control.Lens
import Control.Lens.Regex.ByteString
import qualified Data.ByteString.Char8 as C8 hiding (index)
import Data.Char
import Test.Hspec

spec :: Spec
spec = do
    describe "regex" $ do
        describe "match" $ do
            describe "getting" $ do
                it "should find one match" $ do
                    "abc" ^.. [regex|b|] . match
                    `shouldBe` ["b"]

                it "should find many matches" $ do
                    "a b c" ^.. [regex|\w|] . match
                    `shouldBe` ["a", "b", "c"]

                it "should fold" $ do
                    "a b c" ^. [regex|\w|] . match
                    `shouldBe` "abc"

                it "should match with a group" $ do
                    "a b c" ^.. [regex|(\w)|] . match
                    `shouldBe` ["a", "b", "c"]

                it "should match with many groups" $ do
                    "a b c" ^.. [regex|(\w) (\w)|] . match
                    `shouldBe` ["a b"]

                it "should be greedy when overlapping" $ do
                    "abc" ^.. [regex|\w+|] . match
                    `shouldBe`["abc"]

                it "should respect lazy modifiers" $ do
                    "abc" ^.. [regex|\w+?|] . match
                    `shouldBe`["a", "b", "c"]

            describe "setting" $ do
                it "should allow setting" $ do
                    ("one two three" & [regex|two|] . match .~ "new")
                    `shouldBe` "one new three"

                it "should allow setting many" $ do
                    ("one <two> three" & [regex|\w+|] . match .~ "new")
                    `shouldBe` "new <new> new"

                it "should allow mutating" $ do
                    ("one two three" & [regex|two|] . match %~ (<> "!!"). C8.map toUpper)
                    `shouldBe` "one TWO!! three"

                it "should allow mutating many" $ do
                    ("one two three" & [regex|two|] . match %~ C8.map toUpper)
                    `shouldBe` "one TWO three"

        describe "indexed" $ do
            it "should allow folding with index" $ do
                ("one two three" ^.. ([regex|\w+|] <. match) . withIndex)
                `shouldBe` [(0, "one"), (1, "two"), (2, "three")]

            it "should allow getting with index" $ do
                ("one two three" ^.. [regex|\w+|] . index 1 . match)
                `shouldBe` ["two"]

            it "should allow setting with index" $ do
                ("one two three" & [regex|\w+|] <. match .@~ C8.pack . show)
                `shouldBe` "0 1 2"

            it "should allow mutating with index" $ do
                ("one two three" & [regex|\w+|] <. match %@~ \i s -> (C8.pack $ show i) <> ": " <> s)
                `shouldBe` "0: one 1: two 2: three"

    describe "groups" $ do
        describe "getting" $ do
            it "should get groups" $ do
                "a b c" ^.. [regex|(\w)|] . groups
                `shouldBe` [["a"], ["b"], ["c"]]

            it "should get multiple groups" $ do
                "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . groups
                `shouldBe` [["raindrops","roses"],["whiskers","kittens"]]

            it "should allow getting a specific index" $ do
                ("one two three four" ^.. [regex|(\w+) (\w+)|] . groups . ix 1)
                `shouldBe` ["two", "four"]

            it "should handle weird group alternation" $ do
                ("AB" ^.. [regex|A(x)?(B)|] . groups `shouldBe` [["", "B"]])
                ("B" ^.. [regex|(A)|(B)|] . groups `shouldBe` [["", "B"]])
                -- This behaviour is consistent with pcre-heavy
                ("A" ^.. [regex|(A)|(B)|] . groups `shouldBe` [["A"]])

        describe "setting" $ do
            it "should allow setting groups as a list" $ do
                ("one two three" & [regex|(\w+) (\w+)|] . groups .~ ["1", "2"])
                `shouldBe` "1 2 three"

            it "should allow editing when result list is the same length" $ do
                ("raindrops on roses and whiskers on kittens" & [regex|(\w+) on (\w+)|] . groups %~ reverse)
                `shouldBe` "roses on raindrops and kittens on whiskers"

    describe "group" $ do
        it "should get a single group" $ do
                "a:b c:d" ^.. [regex|(\w):(\w)|] . group 1
                `shouldBe` ["b", "d"]

        it "should set a single group" $ do
                "a:b c:d" & [regex|(\w):(\w)|] . group 1 %~ C8.map toUpper
                `shouldBe` "a:B c:D"

        describe "traversed" $ do
            it "should allow setting all group matches" $ do
                ("one two three" & [regex|(\w+) (\w+)|] . groups . traversed .~ "new")
                `shouldBe` "new new three"

            it "should allow mutating" $ do
                ("one two three four" & [regex|one (two) (three)|] . groups . traversed %~ (<> "!!") . C8.map toUpper)
                `shouldBe` "one TWO!! THREE!! four"

            it "should allow folding with index" $ do
                ("one two three four" ^.. [regex|(\w+) (\w+)|] . groups . traversed . withIndex)
                `shouldBe` [(0, "one"), (1, "two"), (0, "three"), (1, "four")]

            it "should allow setting with index" $ do
                ("one two three four" & [regex|(\w+) (\w+)|] . groups . traversed .@~ C8.pack . show)
                `shouldBe` "0 1 0 1"

            it "should allow mutating with index" $ do
                ("one two three four" & [regex|(\w+) (\w+)|] . groups . traversed %@~ \i s -> (C8.pack $ show i) <> ": " <> s)
                `shouldBe` "0: one 1: two 0: three 1: four"

            it "should compose indices with matches" $ do
                ("one two three four" ^.. ([regex|(\w+) (\w+)|] <.> groups . traversed) . withIndex)
                `shouldBe` [((0, 0), "one"), ((0, 1), "two"), ((1, 0), "three"), ((1, 1), "four")]

    describe "matchAndGroups" $ do
        it "should get match and groups" $ do
            "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . matchAndGroups
            `shouldBe` [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]

