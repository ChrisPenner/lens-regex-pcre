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
    , Regex
    ) where

import Data.Text as T hiding (index)
import Text.Regex.PCRE.Heavy
import Control.Lens hiding (re, matching)
import Data.Data (Data)
import Data.Data.Lens (biplate)
import Language.Haskell.TH.Quote

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Data.Text.Lens (unpacked)
-- >>> import Data.List (sort)

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
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups
-- [["raindrops","roses"],["whiskers","kittens"]]
--
-- You can access a specific group by combining with `ix`
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . groups .  ix 1
-- ["roses","kittens"]
--
-- @groups@ is a traversal; you can mutate matches through it.
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
groups :: Traversal' Match [T.Text]
groups = partsOf (traversed . _Right)

-- | Traverse each match
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
match :: Traversal' Match T.Text
match f grps = (:[]) . Right <$> f (grps ^. traversed . chosen)

-- | 'regex' but indexed with match number
iregex :: Regex -> IndexedTraversal' Int T.Text Match
iregex pattern = indexing (regex pattern)

-- | The base combinator for doing regex searches.
-- It's a traversal which selects 'Match'es; you can compose it with 'match' or 'groups'
-- to get the relevant parts of your match.
--
-- >>> txt = "raindrops on roses and whiskers on kittens" :: Text
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
-- >>> txt & regex [rx|(\w+) on (\w+)|] . groups %~ Prelude.reverse
-- "roses on raindrops and kittens on whiskers"
--
-- Get the third match
--
-- >>> txt ^? iregex [rx|\w+|] . index 2 . match
-- Just "roses"
--
-- Match integers, 'Read' them into ints, then sort them in-place
-- dumping them back into the source text afterwards.
--
-- >>> "Monday: 29, Tuesday: 99, Wednesday: 3" & partsOf (iregex [rx|\d+|] . match . unpacked . _Show @Int) %~ sort
-- "Monday: 3, Tuesday: 29, Wednesday: 99"
--
-- To alter behaviour of the regex you may wish to pass 'PCREOption's when compiling it.
-- The default behaviour may seem strange in certain cases; e.g. it operates in 'single-line'
-- mode.
--
-- You can make your own version of the QuasiQuoter with any options you want embedded
-- by using 'mkRegexQQ'.
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
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Getter Match (T.Text, [T.Text])
matchAndGroups = to $ \m -> (matchText m, m ^. groups)

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withGroups <. match) . withIndex
-- [(["raindrops","roses"],"raindrops on roses"),(["whiskers","kittens"],"whiskers on kittens")]
--
withMatch :: IndexedTraversal' T.Text Match Match
withMatch p mtch = indexed p (matchText mtch) mtch

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withMatch <. groups) . withIndex
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
withGroups :: IndexedTraversal' [T.Text] Match Match
withGroups p mtch = indexed p (mtch ^. groups) mtch

-- split up text into matches paired with groups; Left is unmatched text
splitter :: Text -> [(MatchRange, GroupRanges)] -> [Either T.Text (T.Text, GroupRanges)]
splitter t [] = wrapIfNotEmpty t
splitter t (((start, end), grps) : rest) =
    splitOnce t ((start, end), grps)
    <> splitter (T.drop end t) (subtractFromAll end rest)

splitOnce :: Text -> (MatchRange, GroupRanges) -> [Either T.Text (T.Text, GroupRanges)]
splitOnce t ((start, end), grps) = do
    let (before, mid) = T.splitAt start t
    let focused = T.take (end - start) mid
    wrapIfNotEmpty before <> [Right (focused, subtractFromAll start grps)]

splitAgain :: (T.Text, GroupRanges) -> Match
splitAgain (t, []) | T.null t = []
                   | otherwise = [Left t]
splitAgain (t, (start, end) : rest) = do
    let (before, mid) = T.splitAt start t
    let focused = T.take (end - start) mid
    wrapIfNotEmpty before
        <> [Right focused]
        <> splitAgain ((T.drop end t), (subtractFromAll end rest))

--- helpers
subtractFromAll :: (Data b) => Int -> b -> b
subtractFromAll n = biplate -~ n

wrapIfNotEmpty :: Text -> [Either Text a]
wrapIfNotEmpty txt
    | T.null txt = []
    | otherwise = [Left txt]
