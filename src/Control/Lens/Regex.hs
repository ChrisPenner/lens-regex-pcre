{-|
Module      : Control.Lens.Regex
Description : PCRE regex combinators for interop with lens
Copyright   : (c) Chris Penner, 2019
License     : BSD3

Note that all traversals in this library are not techically lawful; the break the 'multi-set'
idempotence law; in reality this isn't usually a problem; but consider yourself warned. Test your code.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Regex
    ( regex
    , iregex
    , match
    , groups
    , igroups
    , grouped
    , matchAndGroups

    -- * QuasiQuoter
    , rx
    , Match
    ) where

import Data.Text as T hiding (index)
import Text.Regex.PCRE.Heavy
import Control.Lens hiding (re, matching)
import Language.Haskell.TH.Quote

-- | Match represents a whole regex match; you can drill into it using 'match' or 'groups'
type Match = [Either Text Text]
type MatchRange = (Int, Int)
type GroupRanges = [(Int, Int)]

-- | 'QuasiQuoter' for compiling regexes.
-- This is just 're' re-exported under a different name so as not to conflict with @re@ from
-- 'Control.Lens'
rx :: QuasiQuoter
rx = re

-- | 'groups' but indexed by the group number. If you traverse over many matches you will
-- encounter duplicate indices.
-- E.g.
--
-- > > "a 1 b 2" ^.. regex [rx|(\w) (\d)|] . igroups . withIndex
-- > [(0,"a"),(1,"1"),(0,"b"),(1,"2")]
--
-- If you want only a specific group; combine this with `index`
-- E.g.
--
-- > > "a 1 b 2" ^.. regex [rx|(\w) (\d)|] . igroups . index 0
-- > ["a","b"]
igroups :: IndexedTraversal' Int Match T.Text
igroups = indexing groups

-- | traverse each group within a match. See 'igroups' for selecting specific groups or
-- 'grouped' for handling all groups at once.
groups :: Traversal' Match T.Text
groups = traversed . _Right

-- | Access all groups of a match at once.
--
-- Note that this uses 'partsOf'; and is only a valid traversal if you don't
-- alter the length of the list. It is valid as a Fold or Getter however.
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . grouped
-- > [["raindrops","roses"],["whiskers","kittens"]]
--
-- > > "raindrops on roses and whiskers on kittens" & regex [rx|(\w+) on (\w+)|] . grouped %~ reverse
-- > "roses on raindrops and kittens on whiskers"
grouped :: Traversal' Match [T.Text]
grouped = partsOf (traversed . _Right)

-- | Traverse each match as a whole
--
-- Use with 'regex' or 'iregex'
--
-- > > "one _two_ three _four_" ^.. regex [rx|_\w+_|] . match
-- > ["_two_","_four_"]
--
-- You can edit the traversal to perform a regex replace/substitution
--
-- > > "one _two_ three _four_" & regex [rx|_\w+_|] . match %~ T.toUpper
-- > "one _TWO_ three _FOUR_"
match :: Traversal' Match T.Text
match f grps = (:[]) . Right <$> f (grps ^. traversed . chosen)

-- | Indexed version of 'regex'.
iregex :: Regex -> IndexedTraversal' Int T.Text Match
iregex pattern = indexing (regex pattern)

-- | The base combinator for doing regex searches.
-- It's a traversal which selects 'Match'es; you can compose it with 'match' or 'groups'
-- to get the relevant parts of your match.
--
-- Getting all matches:
--
-- > > "one _two_ three _four_" ^.. regex [rx|_\w+_|] . match
-- > ["_two_","_four_"]
--
-- Regex replace/mutation
--
-- > > "one _two_ three _four_" & regex [rx|_\w+_|] . match %~ T.toUpper
-- > "one _TWO_ three _FOUR_"
--
-- Getting groups with their group index.
--
-- > > "1/2 and 3/4" ^.. regex [rx|(\d+)/(\d+)|] . igroups . withIndex
-- > [(0,"1"),(1,"2"),(0,"3"),(1,"4")]
--
-- Check for any matches:
--
-- > > has (regex [rx|ne+dle|]) "a needle in a haystack"
-- > True
--
-- Check for matches which also match a predicate:
--
-- > > has (regex [rx|\w+|] . match . filtered ((> 7) . T.length)) "one word here is loooooooong"
-- > True
--
-- Get the third match
--
-- > >  "alpha beta charlie delta" ^? (iregex [rx|\w+|] . index 2 . match)
-- > Just "charlie"
--
-- Replace the third match
--
-- > > "alpha beta charlie delta" & (iregex [rx|\w+|] . index 2 . match) .~ "GAMMA"
-- > "alpha beta GAMMA delta"
--
-- Match integers, 'Read' them into ints, then sort each match in-place
--
-- > > "Monday: 29, Tuesday: 99, Wednesday: 3" & partsOf' (iregex [rx|\d+|] . match . unpacked . _Show @Int) %~ sort
-- > "Monday: 3, Tuesday: 29, Wednesday: 99"
regex :: Regex -> Traversal' T.Text Match
regex pattern f txt =  collapse <$> apply (fmap splitAgain <$> splitter txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = scanRanges pattern txt
    collapse :: [Either Text [Either Text Text]] -> Text
    collapse xs = xs ^. folded . beside id (traversed . chosen)
    -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
    apply xs = xs & traversed . _Right %%~ f

-- | Collect both the match text AND all the matching groups
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- > [ ("raindrops on roses", ["raindrops","roses"])
-- > , ("whiskers on kittens", ["whiskers","kittens"])
-- > ]
matchAndGroups :: Getter Match (T.Text, [T.Text])
matchAndGroups = to $ \m -> (m ^. traversed . chosen, m ^. grouped)

splitter :: Text -> [(MatchRange, GroupRanges)] -> [Either T.Text (T.Text, GroupRanges)]
splitter t [] | T.null t = []
              | otherwise = [Left t]
splitter t (((start, end), grps) : rest) = do
    splitOnce t ((start, end), grps)
        <> splitter (T.drop end t) (rest & traversed . beside both (traversed . both) -~ end)

splitAgain :: (T.Text, GroupRanges) -> Match
splitAgain (t, []) | T.null t = []
                   | otherwise = [Left t]
splitAgain (t, (start, end) : rest) = do
    let (before, mid) = T.splitAt start t
    let focused = T.take (end - start) mid
    wrapIfNotEmpty before
        <> [Right focused]
        <> splitAgain ((T.drop end t), (rest & traversed . both -~ end))

splitOnce :: Text -> (MatchRange, GroupRanges) -> [Either T.Text (T.Text, GroupRanges)]
splitOnce t ((start, end), grps) = do
    let (before, mid) = T.splitAt start t
    let focused = T.take (end - start) mid
    wrapIfNotEmpty before
      <> [Right (focused, grps & traversed . both -~ start)]

wrapIfNotEmpty :: Text -> [Either Text a]
wrapIfNotEmpty txt
    | T.null txt = []
    | otherwise = [Left txt]
