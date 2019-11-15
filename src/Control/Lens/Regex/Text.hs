{-|
Module      : Control.Lens.Regex.Text
Description : Text PCRE Regex library with a lensy interface.
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Regex.Text
    (
    -- * Basics
      regex
    , match
    , groups
    , group
    , namedGroups
    , namedGroup
    , matchAndGroups

    -- * Compiling regexes to Traversals
    , regexing
    , mkRegexTraversalQQ

    -- * Types
    , RBS.Match
    , PCRE.Regex
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as BS
import qualified Text.Regex.PCRE.Heavy as PCRE
import qualified Data.Map as M
import Control.Lens hiding (re, matching)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Control.Lens.Regex.ByteString as RBS

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Data.Text.Lens (unpacked)
-- >>> import qualified Data.Text as T
-- >>> import Data.List (sort)

utf8 :: Iso' T.Text BS.ByteString
utf8 = iso T.encodeUtf8 (T.decodeUtf8With T.lenientDecode)

-- | Builds a traversal over text using a Regex pattern
--
-- It's a 'TH.QuasiQuoter' which creates a Traversal out of the given regex string.
-- It's equivalent to calling 'regexing' on a 'PCRE.Regex' created using the
-- 'PCRE.re' QuasiQuoter.
--
-- The "real" type is:
--
-- > regex :: Regex -> IndexedTraversal' Int T.Text Match
--
-- It's a traversal which selects 'RBS.Match'es; compose it with 'match' or 'groups'
-- to get the relevant parts of your match.
--
-- >>> txt = "raindrops on roses and whiskers on kittens"
--
-- Search
--
-- >>> has ([regex|whisk|]) txt
-- True
--
-- Get matches
--
-- >>> txt ^.. [regex|\br\w+|] . match
-- ["raindrops","roses"]
--
-- Edit matches
--
-- >>> txt & [regex|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
-- "R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"
--
-- Get Groups
--
-- >>> txt ^.. [regex|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- Edit Groups
--
-- >>> txt & [regex|(\w+) on (\w+)|] . groups %~ reverse
-- "roses on raindrops and kittens on whiskers"
--
-- Get the third match
--
-- >>> txt ^? [regex|\w+|] . index 2 . match
--Just "roses"
--
-- Edit matches
--
-- >>> txt & [regex|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
-- "R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"
--
-- Get Groups
--
-- >>> txt ^.. [regex|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- Edit Groups
--
-- >>> txt & [regex|(\w+) on (\w+)|] . groups %~ reverse
-- "roses on raindrops and kittens on whiskers"
--
-- Get the third match
--
-- >>> txt ^? [regex|\w+|] . index 2 . match
-- Just "roses"
--
-- Match integers, 'Read' them into ints, then sort them in-place
-- dumping them back into the source text afterwards.
--
-- >>> "Monday: 29, Tuesday: 99, Wednesday: 3" & partsOf ([regex|\d+|] . match . unpacked . _Show @Int) %~ sort
-- "Monday: 3, Tuesday: 29, Wednesday: 99"
--
-- To alter behaviour of the regex you may wish to pass 'PCRE.PCREOption's when compiling it.
-- The default behaviour may seem strange in certain cases; e.g. it operates in 'single-line'
-- mode. You can 'PCRE.compile' the 'PCRE.Regex' separately and add any options you like, then pass the resulting
-- 'PCRE.Regex' into 'regex';
-- Alternatively can make your own version of the QuasiQuoter with any options you want embedded
-- by using 'PCRE.mkRegexQQ'.
-- regex :: Regex -> IndexedTraversal' Int T.Text RBS.Match
regex :: TH.QuasiQuoter
regex = PCRE.re{TH.quoteExp=quoter}
  where
    quoter str = do
        rgx <- TH.quoteExp PCRE.re str
        regexExpr <- TH.varE 'regexing
        return $ TH.AppE regexExpr rgx

-- | Build a QuasiQuoter just like 'regex' but with the provided 'PCRE.PCREOption' overrides.
mkRegexTraversalQQ :: [PCRE.PCREOption] -> TH.QuasiQuoter
mkRegexTraversalQQ opts = (PCRE.mkRegexQQ opts){TH.quoteExp=quoter}
  where
    quoter str = do
        rgx <- TH.quoteExp (PCRE.mkRegexQQ opts) str
        regexExpr <- TH.varE 'regexing
        return $ TH.AppE regexExpr rgx

-- | Build a traversal from the provided 'PCRE.Regex', this is handy if you're QuasiQuoter
-- averse, or if you already have a 'PCRE.Regex' object floating around.
--
-- Also see 'mkRegexTraversalQQ'
regexing :: PCRE.Regex -> IndexedTraversal' Int T.Text RBS.Match
regexing pat = utf8 . RBS.regexing pat

-- | Access all groups of a match as a list. Also keeps full match text as the index in case
-- you need it.
--
-- Note that you can edit the groups through this lens,
-- Changing the length of the list has behaviour similar to 'partsOf'.
--
-- Get all matched groups:
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- You can access a specific group combining with 'ix', or just use 'group' instead
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . groups .  ix 1
-- ["roses","kittens"]
--
-- Editing groups:
--
-- >>> "raindrops on roses and whiskers on kittens" & [regex|(\w+) on (\w+)|] . groups .  ix 1 %~ T.toUpper
-- "raindrops on ROSES and whiskers on KITTENS"
--
-- Editing the list rearranges groups
--
-- >>> "raindrops on roses and whiskers on kittens" & [regex|(\w+) on (\w+)|] . groups %~ Prelude.reverse
-- "roses on raindrops and kittens on whiskers"
--
-- You can traverse the list to flatten out all groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . groups . traversed
-- ["raindrops","roses","whiskers","kittens"]
--
-- This replaces each group with the full match text wrapped in parens:
--
-- >>> "one-two" & [regex|(\w+)-(\w+)|] . groups <. traversed %@~ \mtch grp -> grp <> ":(" <> mtch <> ")"
-- "one:(one-two)-two:(one-two)"
groups :: IndexedLens' T.Text RBS.Match [T.Text]
groups = reindexed (view $ from utf8) RBS.groups <. mapping (from utf8)

-- | Access a specific group of a match. Numbering starts at 0.
--
-- Stashes the full match text as the index in case you need it.
--
-- See 'groups' for more info on grouping
--
-- >>> "key:value, a:b" ^.. [regex|(\w+):(\w+)|] . group 0
-- ["key","a"]
--
-- >>> "key:value, a:b" ^.. [regex|(\w+):(\w+)|] . group 1
-- ["value","b"]
--
-- >>> "key:value, a:b" & [regex|(\w+):(\w+)|] . group 1 %~ T.toUpper
-- "key:VALUE, a:B"
--
-- >>> "key:value, a:b" & [regex|(\w+):(\w+)|] . group 1 %~ T.toUpper
-- "key:VALUE, a:B"
--
-- Replace the first capture group with the full match:
--
-- >>> "a, b" & [regex|(\w+), (\w+)|] . group 0 .@~ \i -> "(" <> i <> ")"
-- "(a, b), b"
group :: Int -> IndexedTraversal' T.Text RBS.Match T.Text
group n = groups <. ix n

-- | Access all the named groups of a match as a 'M.Map'. Stashes the full match text as the index in case
-- you need it.
--
-- Note that you can edit the groups through this lens, but the behaviour is undefined when editing inner elements of __nested__ groups.
-- Behaviour is undefined if groups are removed from the map (so don't do that).
--
-- NOTE: There's currently some strange behaviour in pcre-heavy where trailing unmatched optional groups are omitted, I'm looking into getting that patched, but for now, note the difference in behaviour:
--
-- >>> "A" ^? [regex|(?<a>A)|(?<b>B)|] . namedGroups
-- Just (fromList [("a","A")])
--
-- >>> "B" ^? [regex|(?<a>A)|(?<b>B)|] . namedGroups
-- Just (fromList [("a",""),("b","B")])
--
-- Get all matched groups:
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(?<first>\w+) on (?<second>\w+)|] . namedGroups
-- [fromList [("first","raindrops"),("second","roses")],fromList [("first","whiskers"),("second","kittens")]]
--
-- You can access a specific group combining with 'ix', or just use 'namedGroup' instead
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(?<first>\w+) on (?<second>\w+)|] . namedGroups .  ix "second"
-- ["roses","kittens"]
--
-- Editing groups:
--
-- >>> "raindrops on roses and whiskers on kittens" & [regex|(?<first>\w+) on (?<second>\w+)|] . namedGroups . ix "second" %~ T.toUpper
-- "raindrops on ROSES and whiskers on KITTENS"
--
-- Use indexed helpers to access the full match when operating on a group.
--
-- This replaces the "first" group with the full match text wrapped in parens:
--
-- >>> "one-two" & [regex|(?<first>\w+)-(\w+)|] . namedGroups <. ix "first" %@~ \mtch grp -> grp <> ":(" <> mtch <> ")"
-- "one:(one-two)-two"
namedGroups :: IndexedLens' T.Text RBS.Match (M.Map T.Text T.Text)
namedGroups = reindexed (view $ from utf8) RBS.namedGroups <. mapAsTxt
  where
    mapAsTxt :: Iso' (M.Map BS.ByteString BS.ByteString) (M.Map T.Text T.Text)
    mapAsTxt = iso (M.mapKeys (review utf8)) (M.mapKeys (view utf8)) . mapping (from utf8)

-- | Access a specific named group of a match
--
-- See 'namedGroups' for caveats and more info.
--
-- Stashes the full match text as the index in case you need it.
--
-- >>> "key:value, a:b" ^.. [regex|(?<first>\w+):(?<second>\w+)|] . namedGroup "first"
-- ["key","a"]
--
-- >>> "key:value, a:b" ^.. [regex|(?<first>\w+):(?<second>\w+)|] . namedGroup "second"
-- ["value","b"]
--
-- >>> "key:value, a:b" & [regex|(?<first>\w+):(?<second>\w+)|] . namedGroup "second" %~ T.toUpper
-- "key:VALUE, a:B"
--
-- Replace the first capture group with the full match:
--
-- >>> "a, b" & [regex|(?<first>\w+), (?<second>\w+)|] . namedGroup "first" .@~ \i -> "(" <> i <> ")"
-- "(a, b), b"
namedGroup :: T.Text -> IndexedTraversal' T.Text RBS.Match T.Text
namedGroup name = namedGroups <. ix name

-- | Traverse each match
--
-- Stashes any matched groups into the index in case you need them.
--
--  Get a match if one exists:
--
-- >>> "find a needle in a haystack" ^? [regex|n..dle|] . match
-- Just "needle"
--
--  Collect all matches
--
-- >>> "one _two_ three _four_" ^.. [regex|_\w+_|] . match
-- ["_two_","_four_"]
--
-- You can edit the traversal to perform a regex replace/substitution
--
-- >>> "one _two_ three _four_" & [regex|_\w+_|] . match %~ T.toUpper
-- "one _TWO_ three _FOUR_"
--
-- Here we use the group matches stored in the index to form key-value pairs, replacing the entire match.
--
-- >>> "abc-def, ghi-jkl" & [regex|(\w+)-(\w+)|] . match %@~ \[k, v] _ -> "{" <> k <> ":" <> v <> "}"
-- "{abc:def}, {ghi:jkl}"
match :: IndexedTraversal' [T.Text] RBS.Match T.Text
match = reindexed (fmap (view $ from utf8)) RBS.match <. from utf8

-- | Collect both the match text AND all the matching groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Getter RBS.Match (T.Text, [T.Text])
matchAndGroups = to $ \m -> (m ^. match, m ^. groups)
