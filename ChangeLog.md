# Changelog for lens-regex-pcre

# Next

- Handle optional or alternated groups like `pcre-heavy`. This may change group behaviour on regular expressions which had groups with optional groups. E.g.:
    - `A(x)?(B)`
    - `(A)|(B)|(C)`
- Switch `groups` from `IndexedTraversal'` to `IndexedLens'`
- Add `namedGroups` and `namedGroup`

# 1.0.0.0
- Add `regexing` and `makeRegexTraversalQQ`
- Replace `regex` traversal maker with `regex` QuasiQuoter
- Split Control.Lens.Regex into Control.Lens.Regex.Text and Control.Lens.Regex.ByteString
- Move regexBS to `Control.Lens.Regex.ByteString.regex`
- Change whole implementation to use ByteString Builders for a massive speedup
- Monomorphise `Match text` -> `Match`
- Add groups to index of `match` and match to index of `groups` & `group`
- Add `group = groups . ix n` for accessing a single group.

# 0.3.1.0 
- Match -> Match text
- Added regexBS to run regex on ByteStrings directly

# 0.3.0.0 
- Unify `iregex` into `regex` as a single indexed traversal

# 0.2.0.0 
- Unify `grouped`, `groups`, and `igroups` into just `groups` with optional traversal

# 0.1.1.0 
- Adds `grouped` and `matchAndGroups`

# 0.1.0.1 
- Doc fixes

# 0.1.0.0 
- Initial Release
