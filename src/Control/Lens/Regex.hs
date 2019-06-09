{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Regex
    ( re
    , matches
    , match
    , groups
    ) where

import Data.Functor.Selection
import Control.Lens.Selection
import Data.Text as T hiding (index)
import Data.Text.Encoding as T
import Text.Regex.PCRE.Heavy hiding (match)
import Text.Regex.PCRE.Light hiding (match)
import Control.Lens hiding (re, matching)

type Match = [Either Text Text]
type MatchRange = (Int, Int)
type GroupRanges = [(Int, Int)]

-- matching :: Regex -> Traversal' T.Text T.Text
-- -- matching re f txt = matching' re (fmap T.concat . traverse (either pure f)) txt
-- matching re f txt = matching' re . traversed

groups :: IndexedTraversal' Int Match T.Text
groups = indexing (traversed . _Right)

match :: Traversal' Match T.Text
match f groups = (:[]) . Right <$> f (groups ^. traversed . chosen)

imatched :: Regex -> IndexedTraversal' Int T.Text T.Text
imatched re = indexing (matches re)

matches :: Regex -> Traversal' T.Text T.Text
matches re f txt = collapse <$> apply (fmap splitAgain <$> splitter txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = scanRanges re txt
    collapse :: [Either Text Text] -> Text
    collapse xs = xs ^. folded . chosen
    -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
    apply xs = xs & traversed . _Right %%~ f . collapse


imatching :: Regex -> IndexedTraversal' Int T.Text Match
imatching re = indexing (matching re)
matching :: Regex -> Traversal' T.Text Match
matching re f txt =  collapse <$> apply (fmap splitAgain <$> splitter txt matches)
  where
    matches :: [(MatchRange, GroupRanges)]
    matches = scanRanges re txt
    collapse :: [Either Text [Either Text Text]] -> Text
    collapse xs = xs ^. folded . beside id (traversed . chosen)
    -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
    apply xs = xs & traversed . _Right %%~ f


splitter :: Text -> [(MatchRange, GroupRanges)] -> [Either T.Text (T.Text, GroupRanges)]
splitter t [] = [Left t]
splitter t (((start, end), groups) : rest) = do
    splitOnce t ((start, end), groups)
        <> splitter (T.drop end t) (rest & traversed . beside both (traversed . both) -~ end)

splitAgain :: (T.Text, GroupRanges) -> Match
splitAgain (t, []) = [Left t]
splitAgain (t, (start, end) : rest) = do
    let (before, mid) = T.splitAt start t
    let (focused, after) = T.splitAt (end - start) mid
    [Left before, Right (focused)] <> splitAgain ((T.drop end t), (rest & traversed . both -~ end))

splitOnce :: Text -> (MatchRange, GroupRanges) -> [Either T.Text (T.Text, GroupRanges)]
splitOnce t ((start, end), groups) = do
    let (before, mid) = T.splitAt start t
    let (focused, after) = T.splitAt (end - start) mid
    [Left before, Right (focused, groups & traversed . both -~ start)]


-- groups :: Regex -> IndexedTraversal (Int, Int) T.Text T.Text T.Text T.Text
-- groups re = imatching re <.> indexing (traversed . _Right)

-- groups' :: Regex -> Traversal T.Text T.Text Match T.Text
-- groups' re f txt =  collapse <$> apply (fmap splitAgain <$> splitter txt matches)
--   where
--     matches :: [(MatchRange, GroupRanges)]
--     matches = scanRanges re txt
--     collapse :: [Either Text Text] -> Text
--     collapse xs = xs ^. folded . chosen
--     -- apply :: [Either Text [Either Text Text]] -> _ [Either Text [Either Text Text]]
--     apply xs = xs & traversed . _Right %%~ f
