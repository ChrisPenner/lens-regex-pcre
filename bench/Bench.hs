{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Bench where

import Gauge.Benchmark
import Gauge.Main
import Data.ByteString as BS
import Text.Regex.PCRE.Heavy
import Control.Lens
import Control.Lens.Regex.ByteString

main :: IO ()
main = do
    srcFile <- BS.readFile "./bench/data/small-bible.txt"
    defaultMain
      [ bgroup "static pattern search"
        [ bench "pcre-heavy" $ nf (heavySearch [rx|Moses|]) srcFile
        , bench "lens-regex-pcre" $ nf (lensSearch [rx|Moses|]) srcFile
        ]
      , bgroup "complex pattern search"
        [ bench "pcre-heavy" $ nf (heavySearch [rx|l\w+e|]) srcFile
        , bench "lens-regex-pcre" $ nf (lensSearch [rx|l\w+e|]) srcFile
        ]
      , bgroup "simple replacement"
        [ bench "pcre-heavy" $ nf (heavyReplace [rx|Moses|] "Jarvis") srcFile
        , bench "lens-regex-pcre" $ nf (lensReplace [rx|Moses|] "Jarvis") srcFile
        ]
      , bgroup "complex replacement"
        [ bench "pcre-heavy" $ nf (heavyReplace [rx|M\w*s\w*s|] "Jarvis") srcFile
        , bench "lens-regex-pcre" $ nf (lensReplace [rx|M\w*s\w*s|] "Jarvis") srcFile
        ]
      ]

heavySearch :: Regex -> BS.ByteString -> [BS.ByteString]
heavySearch pat src = fst <$> scan pat src

lensSearch :: Regex -> BS.ByteString -> [BS.ByteString]
lensSearch pat src = src ^.. regex pat . match

heavyReplace :: Regex -> BS.ByteString -> BS.ByteString -> BS.ByteString
heavyReplace pat replacement src = gsub pat replacement src

lensReplace :: Regex -> BS.ByteString -> BS.ByteString -> BS.ByteString
lensReplace pat replacement src = src & regex pat . match .~ replacement
