# lens-regex-pcre

* NOTE: I don't promise that this is __fast__ yet;
* NOTE: currently only supports `Text` but should be generalizable to more string-likes; open an issue if you need it

Working with Regexes in Haskell kinda sucks; it's tough to figure out which libs
to use, and even after you pick one it's tough to figure out how to use it.

As it turns out; regexes are a very lens-like tool; Traversals allow you to select
and alter zero or more matches; traversals can even carry indexes so you know which match or group you're working
on.


Note that all traversals in this library are not techically lawful; the break the 'multi-set'
idempotence law; in reality this isn't usually a problem; but consider yourself warned. Test your code.

Here are a few examples:

```haskell
-- Getting all matches:
> "one _two_ three _four_" ^.. regex [rx|_\w+_|] . match
["_two_","_four_"]

-- Regex replace/mutation
> "one _two_ three _four_" & regex [rx|_\w+_|] . match %~ T.toUpper
"one _TWO_ three _FOUR_"

-- Getting groups with their index.
> "1/2 and 3/4" ^.. regex [rx|(\d+)/(\d+)|] . igroups . withIndex
[(0,"1"),(1,"2"),(0,"3"),(1,"4")]

-- Check for any matches:
> has (regex [rx|ne+dle|]) "a needle in a haystack"
True

-- Check for matches which also match a predicate:
> has (regex [rx|\w+|] . match . filtered ((> 7) . T.length)) "one word here is loooooooong"
True

-- Get the third match
>  "alpha beta charlie delta" ^? (iregex [rx|\w+|] . index 2 . match)
Just "charlie"

-- Replace the third match
> "alpha beta charlie delta" & (iregex [rx|\w+|] . index 2 . match) .~ "GAMMA"
"alpha beta GAMMA delta"

-- Sort all matches alphabetically in place
> "*charlie* beta = _alpha_ delta" & partsOf (iregex [rx|[a-z]+|] . match) %~ sort
"*alpha* beta = _charlie_ delta"

-- Match integers, 'Read' them into ints, then sort each match in-place
> "Monday: 29, Tuesday: 99, Wednesday: 3" & partsOf' (iregex [rx|\d+|] . match . unpacked . _Show @Int) %~ sort
"Monday: 3, Tuesday: 29, Wednesday: 99"
```

Basically anything you want to do is possible somehow.

Expected behaviour (and examples) can be found in the test suite:

```haskell
import Control.Lens
import Control.Lens.Regex

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

    it "should allow getting a specific index" $ do
        ("one two three four" ^.. regex [rx|(\w+) (\w+)|] . igroups . index 1)
        `shouldBe` ["two", "four"]

    it "should allow setting with index" $ do
        ("one two three four" & regex [rx|(\w+) (\w+)|] . igroups .@~ pack . show)
        `shouldBe` "0 1 0 1"

    it "should allow mutating with index" $ do
        ("one two three four" & regex [rx|(\w+) (\w+)|] . igroups %@~ \i s -> (pack $ show i) <> ": " <> s)
        `shouldBe` "0: one 1: two 0: three 1: four"

    it "should compose indices with matches" $ do
        ("one two three four" ^.. (iregex [rx|(\w+) (\w+)|] <.> igroups) . withIndex)
        `shouldBe` [((0, 0), "one"), ((0, 1), "two"), ((1, 0), "three"), ((1, 1), "four")]
```
