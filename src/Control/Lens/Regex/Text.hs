{-|
Module      : Control.Lens.Regex
Description : PCRE regex combinators for interop with lens
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
    -- * Combinators
      regex
    , match
    , groups
    , group
    , matchAndGroups

    -- * Compiling regexes
    , rx
    , mkRegexQQ
    , compile
    , compileM

    -- * Types
    , RBS.Match
    , Regex
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as BS
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (compile)
import Control.Lens hiding (re, matching)
import Language.Haskell.TH.Quote

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

-- | The base combinator for doing regex searches.
-- It's a traversal which selects 'Match'es; compose it with 'match' or 'groups'
-- to get the relevant parts of your match.
--
-- >>> txt = "raindrops on roses and whiskers on kittens"
--
-- Search
--
-- >>> has (regex [rx|whisk|]) txt
-- True
--
-- Get matches
--
-- >>> txt ^.. regex [rx|\br\w+|] . match
-- ["raindrops","roses"]
--
-- Edit matches
--
-- >>> txt & regex [rx|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
-- "R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"
--
-- Get Groups
--
-- >>> txt ^.. regex [rx|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- Edit Groups
--
-- >>> txt & regex [rx|(\w+) on (\w+)|] . groups %~ reverse
-- "roses on raindrops and kittens on whiskers"
--
-- Get the third match
--
-- >>> txt ^? regex [rx|\w+|] . index 2 . match
--Just "roses"
--
-- Edit matches
--
-- >>> txt & regex [rx|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
-- "R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"
--
-- Get Groups
--
-- >>> txt ^.. regex [rx|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- Edit Groups
--
-- >>> txt & regex [rx|(\w+) on (\w+)|] . groups %~ reverse
-- "roses on raindrops and kittens on whiskers"
--
-- Get the third match
--
-- >>> txt ^? regex [rx|\w+|] . index 2 . match
-- Just "roses"
--
-- Match integers, 'Read' them into ints, then sort them in-place
-- dumping them back into the source text afterwards.
--
-- >>> "Monday: 29, Tuesday: 99, Wednesday: 3" & partsOf (regex [rx|\d+|] . match . unpacked . _Show @Int) %~ sort
-- "Monday: 3, Tuesday: 29, Wednesday: 99"
--
-- To alter behaviour of the regex you may wish to pass 'PCREOption's when compiling it.
-- The default behaviour may seem strange in certain cases; e.g. it operates in 'single-line'
-- mode. You can 'compile' the 'Regex' separately and add any options you like, then pass the resulting
-- 'Regex' into 'regex';
-- Alternatively can make your own version of the QuasiQuoter with any options you want embedded
-- by using 'mkRegexQQ'.
regex :: Regex -> IndexedTraversal' Int T.Text RBS.Match
regex pat = utf8 . RBS.regex pat

-- | Access all groups of a match as a list. Also keeps full match text as the index in case
-- you need it.
--
-- Note that you can edit the groups through this traversal,
-- Changing the length of the list has behaviour similar to 'partsOf'.
--
-- Get all matched groups:
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- You can access a specific group combining with 'ix', or just use 'group' instead
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups .  ix 1
-- ["roses","kittens"]
--
-- Editing groups:
--
-- >>> "raindrops on roses and whiskers on kittens" & regex [rx|(\w+) on (\w+)|] . groups .  ix 1 %~ T.toUpper
-- "raindrops on ROSES and whiskers on KITTENS"
--
-- Editing the list rearranges groups
--
-- >>> "raindrops on roses and whiskers on kittens" & regex [rx|(\w+) on (\w+)|] . groups %~ Prelude.reverse
-- "roses on raindrops and kittens on whiskers"
--
-- You can traverse the list to flatten out all groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups . traversed
-- ["raindrops","roses","whiskers","kittens"]
--
-- This replaces each group with the full match text wrapped in parens:
--
-- >>> "one-two" & regex [rx|(\w+)-(\w+)|] . groups <. traversed %@~ \mtch grp -> grp <> ":(" <> mtch <> ")"
-- "one:(one-two)-two:(one-two)"
groups :: IndexedTraversal' T.Text RBS.Match [T.Text]
groups = reindexed (view $ from utf8) RBS.groups <. mapping (from utf8)

-- | Access a specific group of a match. Numbering starts at 0.
--
-- Stashes the full match text as the index in case you need it.
--
-- See 'groups' for more info on grouping
--
-- >>> "key:value, a:b" ^.. regex [rx|(\w+):(\w+)|] . group 0
-- ["key","a"]
--
-- >>> "key:value, a:b" ^.. regex [rx|(\w+):(\w+)|] . group 1
-- ["value","b"]
--
-- >>> "key:value, a:b" & regex [rx|(\w+):(\w+)|] . group 1 %~ T.toUpper
-- "key:VALUE, a:B"
--
-- >>> "key:value, a:b" & regex [rx|(\w+):(\w+)|] . group 1 %~ T.toUpper
-- "key:VALUE, a:B"
--
-- Replace the first capture group with the full match:
--
-- >>> "a, b" & regex [rx|(\w+), (\w+)|] . Control.Lens.Regex.ByteString.group 0 .@~ \i -> "(" <> i <> ")"
-- "(a, b), b"
group :: Int -> IndexedTraversal' T.Text RBS.Match T.Text
group n = groups <. ix n

-- | Traverse each match
--
-- Stashes any matched groups into the index in case you need them.
--
--  Get a match if one exists:
--
-- >>> "find a needle in a haystack" ^? regex [rx|n..dle|] . match
-- Just "needle"
--
--  Collect all matches
--
-- >>> "one _two_ three _four_" ^.. regex [rx|_\w+_|] . match
-- ["_two_","_four_"]
--
-- You can edit the traversal to perform a regex replace/substitution
--
-- >>> "one _two_ three _four_" & regex [rx|_\w+_|] . match %~ T.toUpper
-- "one _TWO_ three _FOUR_"
--
-- Here we use the group matches stored in the index to form key-value pairs, replacing the entire match.
--
-- >>> "abc-def, ghi-jkl" & regex [rx|(\w+)-(\w+)|] . match %@~ \[k, v] _ -> "{" <> k <> ":" <> v <> "}"
-- "{abc:def}, {ghi:jkl}"
match :: IndexedTraversal' [T.Text] RBS.Match T.Text
match = reindexed (fmap (view $ from utf8)) RBS.match <. from utf8

-- | Collect both the match text AND all the matching groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Getter RBS.Match (T.Text, [T.Text])
matchAndGroups = to $ \m -> (m ^. match, m ^. groups)

-- | 'QuasiQuoter' for compiling regexes.
-- This is just 're' re-exported under a different name so as not to conflict with @re@ from
-- 'Control.Lens'
rx :: QuasiQuoter
rx = re
