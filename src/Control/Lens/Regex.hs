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

module Control.Lens.Regex
    (
    -- * Combinators
      regex
    , regexBS
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
    , Match
    , Regex
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as BS
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (compile)
import Control.Lens hiding (re, matching)
import Data.Data (Data)
import Data.Data.Lens (biplate)
import Language.Haskell.TH.Quote

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import Data.Text.Lens (unpacked)
-- >>> import Data.Text (Text)
-- >>> import Data.List (sort)

-- | Match represents a whole regex match; you can drill into it using 'match' or 'groups' or 'matchAndGroups'
--
-- @text@ is either "Text" or "ByteString" depending on whether you use 'regex' or 'regexBS'
--
-- Consider this to be internal; don't depend on its representation.
type Match text = [Either text text]
type MatchRange = (Int, Int)
type GroupRanges = [(Int, Int)]

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
-- You can access a specific group combining with 'ix', or just use 'group' instead
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
groups :: Traversal' (Match text) [text]
groups = partsOf (traversed . _Right)

-- | Access a specific group of a match. Numbering starts at 0.
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
group :: Int -> Traversal' (Match text) text
group n = groups . ix n

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
match :: Monoid text => Traversal' (Match text) text
match f grps = (:[]) . Right <$> f (grps ^. traversed . chosen)

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
regex :: Regex -> IndexedTraversal' Int T.Text (Match T.Text)
regex pattern = utf8 . regexBS pattern . matchBsText
  where
    utf8 :: Iso' T.Text BS.ByteString
    utf8 = iso T.encodeUtf8 (T.decodeUtf8With T.lenientDecode)
    matchBsText :: Iso' [Either BS.ByteString BS.ByteString] (Match T.Text)
    matchBsText = iso (traversed . chosen %~ T.decodeUtf8With T.lenientDecode) (traversed . chosen %~ T.encodeUtf8)

-- | A version of 'regex' which operates directly on 'BS.ByteString's.
-- This is more efficient than using 'regex' as it avoids converting back and forth
-- between 'BS.ByteString' and 'T.Text'.
regexBS :: Regex -> IndexedTraversal' Int BS.ByteString (Match BS.ByteString)
regexBS pattern = conjoined (regexT pattern) (indexing (regexT pattern))

-- | Base regex traversal. Used only to define 'regex' traversals
regexT :: Regex -> Traversal' BS.ByteString [Either BS.ByteString BS.ByteString]
regexT pattern f txt = collapseMatch <$> apply (fmap splitAgain <$> splitter txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = scanRanges pattern txt
    collapseMatch :: [Either BS.ByteString [Either BS.ByteString BS.ByteString]] -> BS.ByteString
    collapseMatch xs = xs ^. folded . beside id (traversed . chosen)
    -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
    apply xs = xs & traversed . _Right %%~ f


-- | Get the full match text from a match
matchText :: Monoid text => Match text -> text
matchText m = m ^. traversed . chosen

-- | Collect both the match text AND all the matching groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Monoid text => Getter (Match text) (text, [text])
matchAndGroups = to $ \m -> (matchText m, m ^. groups)

-- | 'QuasiQuoter' for compiling regexes.
-- This is just 're' re-exported under a different name so as not to conflict with @re@ from
-- 'Control.Lens'
rx :: QuasiQuoter
rx = re

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withGroups <. match) . withIndex
-- [(["raindrops","roses"],"raindrops on roses"),(["whiskers","kittens"],"whiskers on kittens")]
withMatch :: Monoid text => IndexedTraversal' text (Match text) (Match text)
withMatch p mtch = indexed p (matchText mtch) mtch

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withMatch <. groups) . withIndex
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
withGroups :: IndexedTraversal' [text] (Match text) (Match text)
withGroups p mtch = indexed p (mtch ^. groups) mtch

-- split up text into matches paired with groups; Left is unmatched text
splitter :: BS.ByteString -> [(MatchRange, GroupRanges)] -> [Either BS.ByteString (BS.ByteString, GroupRanges)]
splitter t [] = wrapIfNotEmpty t
splitter t (((start, end), grps) : rest) =
    splitOnce t ((start, end), grps)
    <> splitter (BS.drop end t) (subtractFromAll end rest)

splitOnce :: BS.ByteString -> (MatchRange, GroupRanges) -> [Either BS.ByteString (BS.ByteString, GroupRanges)]
splitOnce t ((start, end), grps) = do
    let (before, mid) = BS.splitAt start t
    let focused = BS.take (end - start) mid
    wrapIfNotEmpty before <> [Right (focused, subtractFromAll start grps)]

splitAgain :: (BS.ByteString, GroupRanges) -> [Either BS.ByteString BS.ByteString]
splitAgain (t, []) | BS.null t = []
                   | otherwise = [Left t]
splitAgain (t, (start, end) : rest) = do
    let (before, mid) = BS.splitAt start t
    let focused = BS.take (end - start) mid
    wrapIfNotEmpty before
        <> [Right focused]
        <> splitAgain ((BS.drop end t), (subtractFromAll end rest))

--- helpers
subtractFromAll :: (Data b) => Int -> b -> b
subtractFromAll n = biplate -~ n

wrapIfNotEmpty :: BS.ByteString -> [Either BS.ByteString a]
wrapIfNotEmpty txt
    | BS.null txt = []
    | otherwise = [Left txt]
