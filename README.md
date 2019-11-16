# lens-regex-pcre

[Hackage and Docs](http://hackage.haskell.org/package/lens-regex-pcre)

Based on `pcre-heavy`; so it should support any regexes or options which it supports.

Performance is [equal, sometimes **better**](https://github.com/ChrisPenner/lens-regex-pcre#performance) than that of `pcre-heavy` alone.

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
import Control.Lens.Regex.Text

txt :: Text
txt = "raindrops on roses and whiskers on kittens"

-- Search
>>> has [regex|whisk|] txt
True

-- Get matches
>>> txt ^.. [regex|\br\w+|] . match
["raindrops","roses"]

-- Edit matches
>>> txt & [regex|\br\w+|] . match %~ T.intersperse '-' . T.toUpper
"R-A-I-N-D-R-O-P-S on R-O-S-E-S and whiskers on kittens"

-- Get Groups
>>> txt ^.. [regex|(\w+) on (\w+)|] . groups
[["raindrops","roses"],["whiskers","kittens"]]

-- Edit Groups
>>> txt & [regex|(\w+) on (\w+)|] . groups %~ reverse
"roses on raindrops and kittens on whiskers"

-- Get the third match
>>> txt ^? [regex|\w+|] . index 2 . match
Just "roses"

-- Match integers, 'Read' them into ints, then sort them in-place
-- dumping them back into the source text afterwards.
>>> "Monday: 29, Tuesday: 99, Wednesday: 3" 
   & partsOf ([regex|\d+|] . match . unpacked . _Show @Int) %~ sort
"Monday: 3, Tuesday: 29, Wednesday: 99"

```

Basically anything you want to do is possible somehow.

# Performance

See the [benchmarks](https://github.com/ChrisPenner/lens-regex-pcre/blob/master/bench/Bench.hs).

## Summary

Caveat: I'm by no means a benchmarking expert; if you have tips on how to do this better I'm all ears!

* **Search** `lens-regex-pcre` is *marginally* slower than `pcre-heavy`, but well within acceptable margins (within 0.6%)
* **Replace** `lens-regex-pcre` beats `pcre-heavy` by ~10%
* **Modify** `pcre-heavy` doesn't support this operation at all, so I guess `lens-regex-pcre` wins here :)

How can it possibly be **faster** if it's based on `pcre-heavy`? `lens-regex-pcre` only uses `pcre-heavy` for **finding** the matches, not substitution/replacement. After that it splits the text into chunks and traverses over them with whichever operation you've chosen. The nature of this implementation makes it a lot easier to understand than imperative implementations of the same thing. This means it's pretty easy to make edits, and is also the reason we can support arbitrary traversals/actions. It was easy enough, so I went ahead and made the whole thing use ByteString Builders, which sped it up a lot. I suspect that `pcre-heavy` can benefit from the same optimization if anyone feels like back-porting it; it could be (almost) as nicely using simple `traverse` without any lenses. The whole thing is only about 25 LOC.

I'm neither a benchmarks nor stats person, so please open an issue if anything here seems fishy.

Without `pcre-light` and `pcre-heavy` this library wouldn't be possible, so huge thanks to all contributors!

Here are the benchmarks on my 2013 Macbook (2.6 Ghz i5)

```haskell
benchmarking static pattern search/pcre-heavy ... took 20.78 s, total 56 iterations
benchmarked static pattern search/pcre-heavy
time                 375.3 ms   (372.0 ms .. 378.5 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 378.1 ms   (376.4 ms .. 380.8 ms)
std dev              3.747 ms   (922.3 μs .. 5.609 ms)

benchmarking static pattern search/lens-regex-pcre ... took 20.79 s, total 56 iterations
benchmarked static pattern search/lens-regex-pcre
time                 379.5 ms   (376.2 ms .. 382.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 377.3 ms   (376.5 ms .. 378.4 ms)
std dev              1.667 ms   (1.075 ms .. 2.461 ms)

benchmarking complex pattern search/pcre-heavy ... took 95.95 s, total 56 iterations
benchmarked complex pattern search/pcre-heavy
time                 1.741 s    (1.737 s .. 1.746 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.746 s    (1.744 s .. 1.749 s)
std dev              4.499 ms   (3.186 ms .. 6.080 ms)

benchmarking complex pattern search/lens-regex-pcre ... took 97.26 s, total 56 iterations
benchmarked complex pattern search/lens-regex-pcre
time                 1.809 s    (1.736 s .. 1.908 s)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 1.757 s    (1.742 s .. 1.810 s)
std dev              42.83 ms   (11.51 ms .. 70.69 ms)

benchmarking simple replacement/pcre-heavy ... took 23.32 s, total 56 iterations
benchmarked simple replacement/pcre-heavy
time                 423.8 ms   (422.4 ms .. 425.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 424.0 ms   (422.9 ms .. 426.2 ms)
std dev              2.684 ms   (1.239 ms .. 4.270 ms)

benchmarking simple replacement/lens-regex-pcre ... took 20.84 s, total 56 iterations
benchmarked simple replacement/lens-regex-pcre
time                 382.8 ms   (374.3 ms .. 391.5 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 378.2 ms   (376.3 ms .. 381.0 ms)
std dev              3.794 ms   (2.577 ms .. 5.418 ms)

benchmarking complex replacement/pcre-heavy ... took 24.77 s, total 56 iterations
benchmarked complex replacement/pcre-heavy
time                 448.1 ms   (444.7 ms .. 450.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 450.8 ms   (449.5 ms .. 453.9 ms)
std dev              3.129 ms   (947.0 μs .. 4.841 ms)

benchmarking complex replacement/lens-regex-pcre ... took 21.99 s, total 56 iterations
benchmarked complex replacement/lens-regex-pcre
time                 399.9 ms   (398.4 ms .. 402.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 399.6 ms   (399.0 ms .. 400.4 ms)
std dev              1.135 ms   (826.2 μs .. 1.604 ms)

Benchmark lens-regex-pcre-bench: FINISH
```

# Behaviour

Precise Expected behaviour (and examples) can be found in the test suites:

* [ByteString tests](https://github.com/ChrisPenner/lens-regex-pcre/blob/master/test/ByteString.hs)
* [Text tests](https://github.com/ChrisPenner/lens-regex-pcre/blob/master/test/Text.hs)
