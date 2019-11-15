{-|
Module      : Control.Lens.Regex.ByteString
Description : ByteString PCRE Regex library with a lensy interface.
Copyright   : (c) Chris Penner, 2019
License     : BSD3
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Control.Lens.Regex.ByteString
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
    , Match
    , PCRE.Regex
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BS
import qualified Text.Regex.PCRE.Heavy as PCRE
import qualified Text.Regex.PCRE.Light as PCRE
import Control.Lens hiding (re)
import Data.Bifunctor
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH as TH
import GHC.TypeLits
import qualified Data.Map as M
import Data.Tuple (swap)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import qualified Data.ByteString.Char8 as Char8
-- >>> import Data.Char
-- >>> import Data.List hiding (group)
-- >>> import Data.ByteString.Lens

type MatchRange = (Int, Int)
type GroupRanges = [(Int, Int)]

-- | Match represents an opaque regex match.
-- You can drill into it using 'match', 'groups', 'group' or 'matchAndGroups'
data Match =
    Match { _chunks    :: [Either BS.Builder BS.Builder]
          , _matchRegex :: PCRE.Regex
          }
makeLenses ''Match

instance TypeError
  ('Text "You're trying to 'show' a raw 'Match' object."
   ':$$: 'Text "You likely missed adding a 'match' or 'groups' or 'group' call after your 'regex' call :)")
  => Show Match where
  show _ = "This is a raw Match object, did you miss a 'match' or 'groups' or 'group' call after your 'regex'?"

unBuilder :: BS.Builder -> BS.ByteString
unBuilder = BL.toStrict . BS.toLazyByteString

building :: Iso' BS.Builder BS.ByteString
building = iso unBuilder BS.byteString

-- | Access all groups of a match as a list. Stashes the full match text as the index in case
-- you need it.
--
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
-- >>> "raindrops on roses and whiskers on kittens" & [regex|(\w+) on (\w+)|] . groups .  ix 1 %~ Char8.map toUpper
-- "raindrops on ROSES and whiskers on KITTENS"
--
-- Editing the list rearranges groups
--
-- >>> "raindrops on roses and whiskers on kittens" & [regex|(\w+) on (\w+)|] . groups %~ reverse
-- "roses on raindrops and kittens on whiskers"
--
-- You can traverse the list to flatten out all groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . groups . traversed
-- ["raindrops","roses","whiskers","kittens"]
--
-- Use indexed helpers to access the full match when operating on a group.
--
-- This replaces each group with the full match text wrapped in parens:
--
-- >>> "one-two" & [regex|(\w+)-(\w+)|] . groups <. traversed %@~ \mtch grp -> grp <> ":(" <> mtch <> ")"
-- "one:(one-two)-two:(one-two)"
groups :: IndexedLens' BS.ByteString Match [BS.ByteString]
groups = conjoined groupsT (reindexed (view match) selfIndex <. groupsT)
    where
      groupsT :: Lens' Match [BS.ByteString]
      groupsT = chunks . partsOf (traversed . _Right . building)

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
-- >>> "key:value, a:b" & [regex|(\w+):(\w+)|] . group 1 %~ Char8.map toUpper
-- "key:VALUE, a:B"
--
-- Replace the first capture group with the full match:
--
-- >>> "a, b" & [regex|(\w+), (\w+)|] . group 0 .@~ \i -> "(" <> i <> ")"
-- "(a, b), b"
group :: Int -> IndexedTraversal' BS.ByteString Match BS.ByteString
group n = groups <. ix n

-- | Access all the named groups of a match as a 'M.Map'. Stashes the full match text as the index in case
-- you need it.
--
-- Note that you can edit the groups through this lens,
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
-- >>> "raindrops on roses and whiskers on kittens" & [regex|(?<first>\w+) on (?<second>\w+)|] . namedGroups . ix "second" %~ Char8.map toUpper
-- "raindrops on ROSES and whiskers on KITTENS"
--
-- Use indexed helpers to access the full match when operating on a group.
--
-- This replaces the "first" group with the full match text wrapped in parens:
--
-- >>> "one-two" & [regex|(?<first>\w+)-(\w+)|] . namedGroups <. ix "first" %@~ \mtch grp -> grp <> ":(" <> mtch <> ")"
-- "one:(one-two)-two"
namedGroups :: IndexedLens' BS.ByteString Match (M.Map BS.ByteString BS.ByteString)
namedGroups = conjoined stepOne (reindexed (view match) selfIndex <. stepOne)
    where
      -- stepOne :: Traversal' Match (M.Map BS.ByteString BS.ByteString)
      stepOne :: Lens' Match (M.Map BS.ByteString BS.ByteString)
      stepOne f m = m & (groups . zipT . converterT (_matchRegex m) . partsOf (traversed . _Right) . mapL) %%~ f
      zipT :: Iso' [a]  [(Int, a)]
      zipT = iso (zip [0..]) (fmap snd)
      converterT :: PCRE.Regex -> Lens' [(Int, BS.ByteString)] [Either (Int, BS.ByteString) (BS.ByteString, BS.ByteString)]
      converterT pattern f xs =
          f (converter pattern xs) <&> itraversed %@~ \i l -> either id ((i,) . snd) l
      converter :: PCRE.Regex -> [(Int, BS.ByteString)] -> [Either (Int, BS.ByteString) (BS.ByteString, BS.ByteString)]
      converter pattern = fmap $ \(i, s) ->
          case M.lookup i (names pattern) of
              Nothing -> Left (i, s)
              Just n -> Right (n, s)
      mapL :: Lens' [(BS.ByteString, BS.ByteString)] (M.Map BS.ByteString BS.ByteString)
      mapL = lens M.fromList setter
        where
          setter :: [(BS.ByteString, BS.ByteString)] -> M.Map BS.ByteString BS.ByteString -> [(BS.ByteString, BS.ByteString)]
          setter xs m = xs <&> \(k, _) -> (k, M.findWithDefault "" k m)
      names :: PCRE.Regex -> M.Map Int BS.ByteString
      names pattern = M.fromList . fmap swap $ PCRE.captureNames pattern

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
-- >>> "key:value, a:b" & [regex|(?<first>\w+):(?<second>\w+)|] . namedGroup "second" %~ Char8.map toUpper
-- "key:VALUE, a:B"
--
-- Replace the first capture group with the full match:
--
-- >>> "a, b" & [regex|(?<first>\w+), (?<second>\w+)|] . namedGroup "first" .@~ \i -> "(" <> i <> ")"
-- "(a, b), b"
namedGroup :: BS.ByteString -> IndexedTraversal' BS.ByteString Match BS.ByteString
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
-- >>> "one _two_ three _four_" & [regex|_\w+_|] . match %~ Char8.map toUpper
-- "one _TWO_ three _FOUR_"
--
-- Here we use the group matches stored in the index to form key-value pairs, replacing the entire match.
--
-- >>> "abc-def, ghi-jkl" & [regex|(\w+)-(\w+)|] . match %@~ \[k, v] _ -> "{" <> k <> ":" <> v <> "}"
-- "{abc:def}, {ghi:jkl}"
match :: IndexedTraversal' [BS.ByteString] Match BS.ByteString
match = conjoined matchBS (reindexed (view groups) selfIndex <. matchBS)
  where
    matchBS :: Traversal' Match BS.ByteString
    matchBS = chunks . matchT . building
    matchT :: Traversal' [Either BS.Builder BS.Builder] BS.Builder
    matchT f grps =
        (:[]) . Right <$> f (grps ^. folded . chosen)

-- | Build a traversal from the provided 'PCRE.Regex', this is handy if you're QuasiQuoter
-- averse, or if you already have a 'PCRE.Regex' object floating around.
--
-- Also see 'mkRegexTraversalQQ'
regexing :: PCRE.Regex -> IndexedTraversal' Int BS.ByteString Match
regexing pattern = conjoined (regexT pattern) (indexing (regexT pattern)) . asMatch
  where
    -- Unlawful iso, but since the Regex field of Match isn't exported it's fine.
    asMatch :: Iso' [Either BS.Builder BS.Builder] Match
    asMatch = iso to' from'
    to' xs = Match xs pattern
    from' (Match xs _) = xs

-- | Base regex traversal helper
regexT :: PCRE.Regex -> Traversal' BS.ByteString [Either BS.Builder BS.Builder]
regexT pattern f txt = unBuilder . collapseMatch <$> apply (splitAll txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = PCRE.scanRanges pattern txt
    collapseMatch :: [Either BS.Builder [Either BS.Builder BS.Builder]] -> BS.Builder
    collapseMatch xs = xs ^. folded . beside id (traversed . chosen)
    apply xs = xs & traversed . _Right %%~ f

-- | Collect both the match text AND all the matching groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. [regex|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Getter Match (BS.ByteString, [BS.ByteString])
matchAndGroups = to $ \m -> (m ^. match, m ^. groups)

-- | Builds a traversal over text using a Regex pattern
--
-- It's a 'QuasiQuoter' which creates a Traversal out of the given regex string.
-- It's equivalent to calling 'regexing' on a 'Regex' created using the
-- 're' QuasiQuoter.
--
-- The "real" type is:
--
-- > regex :: Regex -> IndexedTraversal' Int BS.ByteString Match
--
-- It's a traversal which selects 'Match'es; compose it with 'match' or 'groups'
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
-- >>> txt & [regex|\br\w+|] . match %~ Char8.intersperse '-' . Char8.map toUpper
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
-- >>> txt & [regex|\br\w+|] . match %~ Char8.intersperse '-' . Char8.map toUpper
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
-- >>> "Monday: 29, Tuesday: 99, Wednesday: 3" & partsOf ([regex|\d+|] . match . from packedChars . _Show @Int) %~ sort
-- "Monday: 3, Tuesday: 29, Wednesday: 99"
--
-- To alter behaviour of the regex you may wish to pass 'PCREOption's when compiling it.
-- The default behaviour may seem strange in certain cases; e.g. it operates in 'single-line'
-- mode. You can 'compile' the 'Regex' separately and add any options you like, then pass the resulting
-- 'Regex' into 'regex';
-- Alternatively can make your own version of the QuasiQuoter with any options you want embedded
-- by using 'mkRegexQQ'.
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

---------------------------------------------------------------------------------------------

splitAll :: BS.ByteString -> [(MatchRange, GroupRanges)] -> [Either BS.Builder [Either BS.Builder BS.Builder]]
splitAll txt matches = fmap (second (\(txt', (start,_), grps) -> groupSplit txt' start grps)) splitUp
  where
    splitUp = splits txt 0 matches

groupSplit :: BS.ByteString -> Int -> GroupRanges -> [Either BS.Builder BS.Builder]
groupSplit txt _ [] = [Left $ BS.byteString txt]
groupSplit txt offset ((-1, -1) : rest) = Right "" : groupSplit txt offset rest
groupSplit txt offset ((grpStart, grpEnd) : rest) | offset == grpStart =
    let (prefix, suffix) = BS.splitAt (grpEnd - offset) txt
     in Right (BS.byteString prefix) : groupSplit suffix grpEnd rest
groupSplit txt offset ((grpStart, grpEnd) : rest) =
    let (prefix, suffix) = BS.splitAt (grpStart - offset) txt
     in Left (BS.byteString prefix) : groupSplit suffix grpStart ((grpStart, grpEnd) : rest)

splits :: BS.ByteString -> Int -> [(MatchRange, GroupRanges)] -> [Either BS.Builder (BS.ByteString, MatchRange, GroupRanges)]
-- No more matches left
splits txt _ [] = [Left $ BS.byteString txt]
-- We're positioned at a match
splits txt offset (((start, end), grps) : rest) | offset == start =
    let (prefix, suffix) = BS.splitAt (end - offset) txt
     in (Right (prefix, (start, end), grps)) : splits suffix end rest
-- jump to the next match
splits txt offset matches@(((start, _), _) : _) =
    let (prefix, suffix) = BS.splitAt (start - offset) txt
     in (Left $ BS.byteString prefix) : splits suffix start matches
