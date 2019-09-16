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

module Control.Lens.Regex.ByteString
    (
    -- * Combinators
      regex
    , match
    , groups
    , group
    , matchAndGroups
    , matchText

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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BS
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light (compile)
import Control.Lens hiding (re, matching)
import Data.Data (Data)
import Data.Data.Lens (biplate)
import Data.Bifunctor
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
type Match = [Either BS.Builder BS.Builder]
type MatchRange = (Int, Int)
type GroupRanges = [(Int, Int)]

unBuilder :: BS.Builder -> BS.ByteString
unBuilder = BL.toStrict . BS.toLazyByteString

building :: Iso' BS.Builder BS.ByteString
building = iso unBuilder BS.byteString

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
groups :: Traversal' Match [BS.ByteString]
groups = partsOf (traversed . _Right . building)

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
group :: Int -> Traversal' Match BS.ByteString
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
match :: Traversal' Match BS.ByteString
match f grps = (:[]) . Right . BS.byteString <$> f (unBuilder $ grps ^. folded . chosen)

-- | A version of 'regex' which operates directly on 'BS.ByteString's.
-- This is more efficient than using 'regex' as it avoids converting back and forth
-- between 'BS.ByteString' and 'T.Text'.
regex :: Regex -> IndexedTraversal' Int BS.ByteString Match
regex pattern = conjoined (regexT pattern) (indexing (regexT pattern))

-- | Base regex traversal. Used only to define 'regex' traversals
regexT :: Regex -> Traversal' BS.ByteString [Either BS.Builder BS.Builder]
regexT pattern f txt = unBuilder . collapseMatch <$> apply (splitAll txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = scanRanges pattern txt
    collapseMatch :: [Either BS.Builder [Either BS.Builder BS.Builder]] -> BS.Builder
    collapseMatch xs = xs ^. folded . beside id (traversed . chosen)
    -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
    apply xs = xs & traversed . _Right %%~ f

-- regexT :: Regex -> Traversal' BS.ByteString [Either BS.ByteString BS.ByteString]
-- regexT pattern f txt = collapseMatch <$> apply (fmap splitAgain <$> splitter txt matches)
--   where
--     matches :: [(MatchRange, GroupRanges)]
--     matches = scanRanges pattern txt
--     collapseMatch :: [Either BS.ByteString [Either BS.ByteString BS.ByteString]] -> BS.ByteString
--     collapseMatch xs = xs ^. folded . beside id (traversed . chosen)
--     -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
--     apply xs = xs & traversed . _Right %%~ f


-- | Get the full match text from a match
matchText :: Match -> BS.ByteString
matchText = view match
    -- unBuilder $ m ^. traversed . chosen

-- | Collect both the match text AND all the matching groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Getter Match (BS.ByteString, [BS.ByteString])
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
withMatch :: IndexedTraversal' BS.ByteString Match Match
withMatch p mtch = indexed p (matchText mtch) mtch

-- | This allows you to "stash" the match text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withMatch <. groups) . withIndex
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
withGroups :: IndexedTraversal' [BS.ByteString] Match Match
withGroups p mtch = indexed p (mtch ^. groups) mtch

---------------------------------------------------------------------------------------------

splitAll :: BS.ByteString -> [(MatchRange, GroupRanges)] -> [Either BS.Builder [Either BS.Builder BS.Builder]]
splitAll txt matches = fmap (second (\(txt', (start,_), grps) -> groupSplit txt' start grps)) splitUp
  where
    splitUp = splits txt 0 matches

groupSplit :: BS.ByteString -> Int -> GroupRanges -> [Either BS.Builder BS.Builder]
groupSplit txt _ [] = [Left $ BS.byteString txt]
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
