{-|
Module      : Control.Lens.Regex
Description : PCRE regex combinators for interop with lens
Copyright   : (c) Chris Penner, 2019
License     : BSD3

Note that all traversals in this library are not techically lawful; they break the 'multi-set'
idempotence law; in reality this isn't usually a problem; but consider yourself warned. Test your code.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Regex
    (
    -- * Combinators
      regex
    , iregex
    , match
    , groups
    , matchAndGroups

    -- * QuasiQuoter
    , rx

    -- * Types
    , Match
    ) where

import Data.Text as T hiding (index)
import Text.Regex.PCRE.Heavy
import Control.Lens hiding (re, matching)
import Language.Haskell.TH.Quote

-- | Match represents a whole regex match; you can drill into it using 'match' or 'groups' or
-- 'matchAndGroups'
-- Consider this to be internal; don't depend on its representation.
type Match = [Either Text Text]
type MatchRange = (Int, Int)
type GroupRanges = [(Int, Int)]

-- | 'QuasiQuoter' for compiling regexes.
-- This is just 're' re-exported under a different name so as not to conflict with @re@ from
-- 'Control.Lens'
rx :: QuasiQuoter
rx = re

-- | Access all groups of a match at once.
--
-- Note that you can edit the groups through this traversal,
-- Changing the length of the list has behaviour similar to 'partsOf'.
--
-- Get all matched groups:
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups
-- > [["raindrops","roses"],["whiskers","kittens"]]
--
-- You can access a specific group by combining with `ix`
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups .  ix 1
-- > ["roses", "kittens"]
--
-- @groups@ is a traversal; you can mutate matches through it.
-- > > "raindrops on roses and whiskers on kittens" & regex [rx|(\w+) on (\w+)|] . groups .  ix 1 %~ T.toUpper
-- > "raindrops on ROSES and whiskers on KITTENS"
--
-- Editing the list rearranges groups
--
-- > > "raindrops on roses and whiskers on kittens" & regex [rx|(\w+) on (\w+)|] . groups %~ reverse
-- > "roses on raindrops and kittens on whiskers"
--
-- You can traverse the list to flatten out all groups
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups . traversed
-- > ["raindrops","roses","whiskers","kittens"]
groups :: Traversal' Match [T.Text]
groups = partsOf (traversed . _Right)

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
-- Getting captured groups
--
-- > λ> "1/2 and 3/4" ^.. regex [rx|(\d+)/(\d+)|] . groups
-- > [["1","2"],["3","4"]]
--
-- > λ> "1/2 and 3/4" ^.. regex [rx|(\d+)/(\d+)|] . groups
-- > [["1","2"],["3","4"]]
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
-- > > "alpha beta charlie delta" ^? (iregex [rx|\w+|] . index 2 . match)
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
regex pattern f txt =  collapseMatch <$> apply (fmap splitAgain <$> splitter txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = scanRanges pattern txt
    collapseMatch :: [Either Text [Either Text Text]] -> Text
    collapseMatch xs = xs ^. folded . beside id (traversed . chosen)
    -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
    apply xs = xs & traversed . _Right %%~ f

matchText :: Match -> T.Text
matchText m = m ^. traversed . chosen

-- | Collect both the match text AND all the matching groups
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- > [ ("raindrops on roses", ["raindrops","roses"])
-- > , ("whiskers on kittens", ["whiskers","kittens"])
-- > ]
matchAndGroups :: Getter Match (T.Text, [T.Text])
matchAndGroups = to $ \m -> (matchText m, m ^. groups)

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- > > [(["raindrops","roses"],"raindrops on roses"),(["whiskers","kittens"],"whiskers on kittens")]
-- > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withGroups <. match) . withIndex
withMatch :: IndexedTraversal' T.Text Match Match
withMatch p mtch = indexed p (matchText mtch) mtch

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- > > "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withMatch <. groups) . withIndex
-- > [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
withGroups :: IndexedTraversal' [T.Text] Match Match
withGroups p mtch = indexed p (mtch ^. groups) mtch

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
