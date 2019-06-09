{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens hiding (re)
import Control.Lens.Regex
import Data.Text as T hiding (index)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "regex" $ do
        describe "match" $ do
            describe "getting" $ do
                it "should find one match" $ do
                    "abc" ^.. regex [rx|b|] . match
                    `shouldBe` ["b"]

                it "should find many matches" $ do
                    "a b c" ^.. regex [rx|\w|] . match
                    `shouldBe` ["a", "b", "c"]

                it "should fold" $ do
                    "a b c" ^. regex [rx|\w|] . match
                    `shouldBe` "abc"

                it "should match with a group" $ do
                    "a b c" ^.. regex [rx|(\w)|] . match
                    `shouldBe` ["a", "b", "c"]

                it "should match with many groups" $ do
                    "a b c" ^.. regex [rx|(\w) (\w)|] . match
                    `shouldBe` ["a b"]

                it "should be greedy when overlapping" $ do
                    "abc" ^.. regex [rx|\w+|] . match
                    `shouldBe`["abc"]

                it "should respect lazy modifiers" $ do
                    "abc" ^.. regex [rx|\w+?|] . match
                    `shouldBe`["a", "b", "c"]

            describe "setting" $ do
                it "should allow setting" $ do
                    ("one two three" & regex [rx|two|] . match .~ "new")
                    `shouldBe` "one new three"

                it "should allow setting many" $ do
                    ("one <two> three" & regex [rx|\w+|] . match .~ "new")
                    `shouldBe` "new <new> new"

                it "should allow mutating" $ do
                    ("one two three" & regex [rx|two|] . match %~ (<> "!!"). T.toUpper)
                    `shouldBe` "one TWO!! three"

                it "should allow mutating many" $ do
                    ("one two three" & regex [rx|two|] . match %~ T.toUpper)
                    `shouldBe` "one TWO three"

        describe "groups" $ do
            describe "getting" $ do
                it "should get a group" $ do
                    "a b c" ^.. regex [rx|(\w)|] . groups
                    `shouldBe` ["a", "b", "c"]

                it "should get many groups" $ do
                    "one two three" ^.. regex [rx|(\w+) (\w+)|] . groups
                    `shouldBe` ["one", "two"]

            describe "setting" $ do
                it "should allow setting" $ do
                    ("one two three" & regex [rx|(\w+) (\w+)|] . groups .~ "new")
                    `shouldBe` "new new three"

                it "should allow setting many" $ do
                    ("one two three four" & regex [rx|(\w+) (\w+)|] . groups .~ "new")
                    `shouldBe` "new new new new"

                it "should allow mutating" $ do
                    ("one two three four" & regex [rx|one (two) three|] . groups %~ (<> "!!") . T.toUpper)
                    `shouldBe` "one TWO!! three four"

                it "should allow mutating" $ do
                    ("one two three four" & regex [rx|one (two) (three)|] . groups %~ (<> "!!") . T.toUpper)
                    `shouldBe` "one TWO!! THREE!! four"

    describe "iregex" $ do
        describe "match" $ do
            it "should allow folding with index" $ do
                ("one two three" ^.. (iregex [rx|\w+|] <. match) . withIndex)
                `shouldBe` [(0, "one"), (1, "two"), (2, "three")]

            it "should allow getting with index" $ do
                ("one two three" ^.. iregex [rx|\w+|] . index 1 . match)
                `shouldBe` ["two"]

            it "should allow setting with index" $ do
                ("one two three" & iregex [rx|\w+|] <. match .@~ pack . show)
                `shouldBe` "0 1 2"

            it "should allow mutating with index" $ do
                ("one two three" & iregex [rx|\w+|] <. match %@~ \i s -> (pack $ show i) <> ": " <> s)
                `shouldBe` "0: one 1: two 2: three"

    describe "igroups" $ do
        it "should allow folding with index" $ do
            ("one two three four" ^.. regex [rx|(\w+) (\w+)|] . igroups . withIndex)
            `shouldBe` [(0, "one"), (1, "two"), (0, "three"), (1, "four")]

        it "should allow setting with index" $ do
            ("one two three four" & regex [rx|(\w+) (\w+)|] . igroups .@~ pack . show)
            `shouldBe` "0 1 0 1"

        it "should allow mutating with index" $ do
            ("one two three four" & regex [rx|(\w+) (\w+)|] . igroups %@~ \i s -> (pack $ show i) <> ": " <> s)
            `shouldBe` "0: one 1: two 0: three 1: four"

        it "should compose indices with matches" $ do
            ("one two three four" ^.. (iregex [rx|(\w+) (\w+)|] <.> igroups) . withIndex)
            `shouldBe` [((0, 0), "one"), ((0, 1), "two"), ((1, 0), "three"), ((1, 1), "four")]
