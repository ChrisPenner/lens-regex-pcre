# lens-regex-pcre

[Hackage and Docs](http://hackage.haskell.org/package/lens-regex-pcre)

Based on `pcre-heavy`; so it should support any regexes which it supports.

Working with Regexes in Haskell kinda sucks; it's tough to figure out which libs
to use, and even after you pick one it's tough to figure out how to use it.

As it turns out; regexes are a very lens-like tool; Traversals allow you to select
and alter zero or more matches; traversals can even carry indexes so you know which match or group you're working
on.

Here are a few examples:

```haskell
txt :: Text
txt = "raindrops on roses and whiskers on kittens"

-- Search
λ> has (regex [rx|whisk|]) txt
True

-- Get matches
λ> txt ^.. regex [rx|\br\w+|] . match
["raindrops","roses"]

-- Edit matches
λ> txt & regex [rx|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
"R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"

-- Get Groups
λ> txt ^.. regex [rx|(\w+) on (\w+)|] . groups
[["raindrops","roses"],["whiskers","kittens"]]

-- Edit Groups
λ> txt & regex [rx|(\w+) on (\w+)|] . groups %~ reverse
"roses on raindrops and kittens on whiskers"

-- Get the third match
λ> txt ^? regex [rx|\w+|] . index 2 . match
Just "roses"

-- Match integers, 'Read' them into ints, then sort them in-place
-- dumping them back into the source text afterwards.
λ> "Monday: 29, Tuesday: 99, Wednesday: 3" 
   & partsOf (regex [rx|\d+|] . match . unpacked . _Show @Int) %~ sort
"Monday: 3, Tuesday: 29, Wednesday: 99"

```

Basically anything you want to do is possible somehow.

Expected behaviour (and examples) can be found in the test suite:

```haskell
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

            it "should allow folding with index" $ do
                ("one two three" ^.. (regex [rx|\w+|] <. match) . withIndex)
                `shouldBe` [(0, "one"), (1, "two"), (2, "three")]

            it "should allow getting with index" $ do
                ("one two three" ^.. regex [rx|\w+|] . index 1 . match)
                `shouldBe` ["two"]

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

            it "should allow setting with index" $ do
                ("one two three" & regex [rx|\w+|] <. match .@~ T.pack . show)
                `shouldBe` "0 1 2"

            it "should allow mutating with index" $ do
                ("one two three" & regex [rx|\w+|] <. match %@~ \i s -> (T.pack $ show i) <> ": " <> s)
                `shouldBe` "0: one 1: two 2: three"

describe "groups" $ do
    describe "getting" $ do
        it "should get groups" $ do
            "a b c" ^.. regex [rx|(\w)|] . groups
            `shouldBe` [["a"], ["b"], ["c"]]

        it "should get multiple groups" $ do
            "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups
            `shouldBe` [["raindrops","roses"],["whiskers","kittens"]]

        it "should allow getting a specific index" $ do
            ("one two three four" ^.. regex [rx|(\w+) (\w+)|] . groups . ix 1)
            `shouldBe` ["two", "four"]

    describe "setting" $ do
        it "should allow setting groups as a list" $ do
            ("one two three" & regex [rx|(\w+) (\w+)|] . groups .~ ["1", "2"])
            `shouldBe` "1 2 three"

        it "should allow editing when result list is the same length" $ do
            ("raindrops on roses and whiskers on kittens" & regex [rx|(\w+) on (\w+)|] . groups %~ reverse)
            `shouldBe` "roses on raindrops and kittens on whiskers"

    describe "traversed" $ do
        it "should allow setting all group matches" $ do
            ("one two three" & regex [rx|(\w+) (\w+)|] . groups . traversed .~ "new")
            `shouldBe` "new new three"

        it "should allow mutating" $ do
            ("one two three four" & regex [rx|one (two) (three)|] . groups . traversed %~ (<> "!!") . T.toUpper)
            `shouldBe` "one TWO!! THREE!! four"

        it "should allow folding with index" $ do
            ("one two three four" ^.. regex [rx|(\w+) (\w+)|] . groups . traversed . withIndex)
            `shouldBe` [(0, "one"), (1, "two"), (0, "three"), (1, "four")]

        it "should allow setting with index" $ do
            ("one two three four" & regex [rx|(\w+) (\w+)|] . groups . traversed .@~ T.pack . show)
            `shouldBe` "0 1 0 1"

        it "should allow mutating with index" $ do
            ("one two three four" & regex [rx|(\w+) (\w+)|] . groups . traversed %@~ \i s -> (T.pack $ show i) <> ": " <> s)
            `shouldBe` "0: one 1: two 0: three 1: four"

        it "should compose indices with matches" $ do
            ("one two three four" ^.. (regex [rx|(\w+) (\w+)|] <.> groups . traversed) . withIndex)
            `shouldBe` [((0, 0), "one"), ((0, 1), "two"), ((1, 0), "three"), ((1, 1), "four")]

describe "matchAndGroups" $ do
    it "should get match and groups" $ do
        "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
        `shouldBe` [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
```
