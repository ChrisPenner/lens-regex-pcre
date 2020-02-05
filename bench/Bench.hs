{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Gauge.Benchmark
import Gauge.Main
import Data.ByteString as BS
import qualified Text.Regex.PCRE.Heavy as PCRE
import Control.Lens
import Control.Lens.Regex.ByteString

main :: IO ()
main = do
    srcFile <- BS.readFile "./bench/data/small-bible.txt"
    denseFile <- BS.take 10000 <$> BS.readFile "./bench/data/dense.txt"
    defaultMain
      [ --bgroup "static pattern search"
        -- [ bench "pcre-heavy" $ nf (heavySearch [PCRE.re|Moses|]) srcFile
        -- , bench "lens-regex-pcre" $ nf (lensSearch [PCRE.re|Moses|]) srcFile
        -- ]
      -- , bgroup "complex pattern search"
        -- [ bench "pcre-heavy" $ nf (heavySearch [PCRE.re|l\w+e|]) srcFile
        -- , bench "lens-regex-pcre" $ nf (lensSearch [PCRE.re|l\w+e|]) srcFile
        -- ]
      -- , bgroup "simple replacement"
        -- [ bench "pcre-heavy" $ nf (heavyReplace [PCRE.re|Moses|] "Jarvis") srcFile
        -- , bench "lens-regex-pcre" $ nf (lensReplace [PCRE.re|Moses|] "Jarvis") srcFile
        -- ]
      -- , bgroup "complex replacement"
        -- [ bench "pcre-heavy" $ nf (heavyReplace [PCRE.re|M\w*s\w*s|] "Jarvis") srcFile
        -- , bench "lens-regex-pcre" $ nf (lensReplace [PCRE.re|M\w*s\w*s|] "Jarvis") srcFile
        -- ]
       bgroup "dense replacement"
        [ --bench "pcre-heavy" $ nf (heavyReplace [PCRE.re|M\w*s\w*s|] "Jarvis") srcFile
        bench "lens-regex-pcre" $ nf (lensReplace [PCRE.re|x|] "oo") denseFile
        ]
      ]

heavySearch :: Regex -> BS.ByteString -> [BS.ByteString]
heavySearch pat src = fst <$> PCRE.scan pat src

lensSearch :: Regex -> BS.ByteString -> [BS.ByteString]
lensSearch pat src = src ^.. regexing pat . match

heavyReplace :: Regex -> BS.ByteString -> BS.ByteString -> BS.ByteString
heavyReplace pat replacement src = PCRE.gsub pat replacement src

lensReplace :: Regex -> BS.ByteString -> BS.ByteString -> BS.ByteString
lensReplace pat replacement src = src & regexing pat . match .~ replacement
