# lens-regex-pcre

[Hackage and Docs](http://hackage.haskell.org/package/lens-regex-pcre)

Based on `pcre-heavy`; so it should support any regexes or options which it supports.

Performance is [equal, sometimes **better**](#performance) than that of `pcre-heavy` alone.

Which module should you use?

If you need unicode support, use `Control.Lens.Regex.Text`, if not then `Control.Lens.Regex.ByteString` is faster.

Working with Regexes in Haskell kinda sucks; it's tough to figure out which libs
to use, and even after you pick one it's tough to figure out how to use it; `lens-regex-pcre` hopes to replace most other solutions by being fast, easy to set up, more adaptable with a more consistent interface.

It helps that there are already HUNDREDS of combinators which interop with lenses :smile:.

As it turns out; regexes are a very lens-like tool; Traversals allow you to select
and alter zero or more matches; traversals can even carry indexes so you know which match or group you're working
on.

# Examples

```haskell
txt :: Text
txt = "raindrops on roses and whiskers on kittens"

-- Search
>>> has (regex [rx|whisk|] . match) txt
True

-- Get matches
>>> txt ^.. regex [rx|\br\w+|] . match
["raindrops","roses"]

-- Edit matches
>>> txt & regex [rx|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
"R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"

-- Get Groups
>>> txt ^.. regex [rx|(\w+) on (\w+)|] . groups
[["raindrops","roses"],["whiskers","kittens"]]

-- Edit Groups
>>> txt & regex [rx|(\w+) on (\w+)|] . groups %~ reverse
"roses on raindrops and kittens on whiskers"

-- Get the third match
>>> txt ^? regex [rx|\w+|] . index 2 . match
Just "roses"

-- Match integers, 'Read' them into ints, then sort them in-place
-- dumping them back into the source text afterwards.
>>> "Monday: 29, Tuesday: 99, Wednesday: 3" 
   & partsOf (regex [rx|\d+|] . match . unpacked . _Show @Int) %~ sort
"Monday: 3, Tuesday: 29, Wednesday: 99"

```

Basically anything you want to do is possible somehow.

# Performance

See the [benchmarks](./bench/Bench.hs).

## Summary

* **Search** `lens-regex-pcre` is within margins of equality to `pcre-heavy`
* **Replace** `lens-regex-pcre` beats `pcre-heavy` by a 
* **Replace** `lens-regex-pcre` beats `pcre-heavy` by ~10%

I'm neither a benchmarks nor stats person, so please open an issue if anything here seems fishy.

Here are the benchmarks on my 2013 Macbook (2.6 Ghz i5)

```haskell
benchmarking static pattern search/pcre-heavy ... took 20.70 s, total 56 iterations
benchmarked static pattern search/pcre-heavy
time                 375.9 ms   (374.6 ms .. 377.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 376.3 ms   (375.7 ms .. 376.8 ms)
std dev              842.4 μs   (501.8 μs .. 1.142 ms)

benchmarking static pattern search/lens-regex-pcre ... took 20.90 s, total 56 iterations
benchmarked static pattern search/lens-regex-pcre
time                 381.5 ms   (378.4 ms .. 387.1 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 379.1 ms   (377.4 ms .. 381.2 ms)
std dev              3.415 ms   (2.266 ms .. 5.175 ms)

benchmarking complex pattern search/pcre-heavy ... took 96.61 s, total 56 iterations
benchmarked complex pattern search/pcre-heavy
time                 1.757 s    (1.755 s .. 1.759 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.756 s    (1.755 s .. 1.758 s)
std dev              2.741 ms   (1.879 ms .. 4.478 ms)

benchmarking complex pattern search/lens-regex-pcre ... took 96.61 s, total 56 iterations
benchmarked complex pattern search/lens-regex-pcre
time                 1.755 s    (1.753 s .. 1.759 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.757 s    (1.755 s .. 1.758 s)
std dev              2.069 ms   (1.533 ms .. 2.619 ms)

benchmarking simple replacement/pcre-heavy ... took 23.82 s, total 56 iterations
benchmarked simple replacement/pcre-heavy
time                 430.8 ms   (425.6 ms .. 436.0 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 433.5 ms   (431.1 ms .. 437.8 ms)
std dev              5.558 ms   (2.552 ms .. 8.918 ms)

benchmarking simple replacement/lens-regex-pcre ... took 20.82 s, total 56 iterations
benchmarked simple replacement/lens-regex-pcre
time                 378.1 ms   (377.1 ms .. 378.8 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 378.6 ms   (378.2 ms .. 379.5 ms)
std dev              1.105 ms   (354.0 μs .. 1.736 ms)

benchmarking complex replacement/pcre-heavy ... took 25.16 s, total 56 iterations
benchmarked complex replacement/pcre-heavy
time                 454.7 ms   (446.8 ms .. 459.2 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 457.4 ms   (454.7 ms .. 464.8 ms)
std dev              7.455 ms   (2.088 ms .. 11.90 ms)

benchmarking complex replacement/lens-regex-pcre ... took 22.06 s, total 56 iterations
benchmarked complex replacement/lens-regex-pcre
time                 399.9 ms   (398.8 ms .. 401.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 401.1 ms   (400.2 ms .. 402.2 ms)
std dev              1.688 ms   (1.249 ms .. 2.315 ms)
```

# Behaviour

Precise Expected behaviour (and examples) can be found in the test suites:

* [ByteString tests](./test/ByteString.hs)
* [Text tests](./test/Text.hs)
