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
-- >>> import Data.T.Text.Lens (unpacked)
-- >>> import Data.T.Text (T.Text)
-- >>> import Data.List (sort)

utf8 :: Iso' T.Text BS.ByteString
utf8 = iso T.encodeUtf8 (T.decodeUtf8With T.lenientDecode)

matchText :: RBS.Match -> T.Text
matchText = view (RBS.match . from utf8)

regex :: Regex -> IndexedTraversal' Int T.Text RBS.Match
regex pat = utf8 . RBS.regex pat

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
groups :: Traversal' RBS.Match [T.Text]
groups = RBS.groups . mapping (from utf8)

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
group :: Int -> Traversal' RBS.Match T.Text
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
match :: Traversal' RBS.Match T.Text
match = RBS.match . from utf8

-- | Collect both the match text AND all the matching groups
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . matchAndGroups
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
matchAndGroups :: Getter RBS.Match (T.Text, [T.Text])
matchAndGroups = to $ \m -> (matchText m, m ^. groups)

-- | 'QuasiQuoter' for compiling regexes.
-- This is just 're' re-exported under a different name so as not to conflict with @re@ from
-- 'Control.Lens'
rx :: QuasiQuoter
rx = re

-- | This allows you to "stash" the match T.Text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withGroups <. match) . withIndex
-- [(["raindrops","roses"],"raindrops on roses"),(["whiskers","kittens"],"whiskers on kittens")]
withMatch :: IndexedTraversal' T.Text RBS.Match RBS.Match
withMatch p mtch = indexed p (matchText mtch) mtch

-- | This allows you to "stash" the match T.Text into an index for use later in the traversal.
-- This is a slight abuse of indices; but it can sometimes be handy. This allows you to
-- have the full match in scope when editing groups using indexed combinators.
--
-- If you're viewing or folding you should probably just use 'matchAndGroups'.
--
-- >>> "raindrops on roses and whiskers on kittens" ^.. regex [rx|(\w+) on (\w+)|] . (withMatch <. groups) . withIndex
-- [("raindrops on roses",["raindrops","roses"]),("whiskers on kittens",["whiskers","kittens"])]
withGroups :: IndexedTraversal' [T.Text] RBS.Match RBS.Match
withGroups p mtch = indexed p (mtch ^. groups) mtch
