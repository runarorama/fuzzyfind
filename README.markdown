# fuzzyfind

A package that provides an API for fuzzy text search in Haskell, using a modified version of the [Smith-Waterman algorithm](https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm). The search is intended to behave similarly to the excellent [`fzf` tool by Junegunn Choi](https://github.com/junegunn/fzf).

The core functionality of the library is provided by the `bestMatch` function:

```haskell
bestMatch :: String -> String -> Maybe Alignment
```

Calling `bestMatch query string` will return `Nothing` if `query` is not a subsequence of `string`. Otherwise, it will return the "best" way to line the characters in `query` up with the characters in `string`. Lower-case characters in the query are assumed to be case-insensitive, and upper-case characters are assumed to be case-sensitive.

For example:

```
> bestMatch "ff" "FuzzyFind"
Just (Alignment {score = 25, result = Result {[Match "F", Gap "uzzy", Match "F", Gap "ind"]}})
```

The `score` indicates how "good" the match is. Better matches have higher scores. There's no maximum score (except for the upper limit of the `Int` datatype), but the lowest score is `0`.

A substring from the query will generate a `Match`, and any characters from the input that don't result in a `Match` will generate a `Gap`. Concatenating all the `Match` and `Gap` results should yield the original input string.

Note that the matched characters in the input always occur in the same order as they do in the query pattern.

The algorithm prefers (and will generate higher scores for) the following kinds of matches:

1. Contiguous characters from the query string. For example, `bestMatch "pp"` will find the last two `p`s in `"pickled pepper"`.
2. Characters at the beginnings of words. For example, `bestMatch "pp"` will find the first two `P`s in `"Peter Piper"`.
3. A character in the input that matches the first character of the query pattern is strongly preferred. For example, `bestMatch "mn" "Bat Man"` will score higher than `bestMatch "mn" "Batman"`.

All else being equal, matches that occur later in the input string are preferred.

The `fuzzyFind` function finds input strings that match all the given input patterns. For each input that matches it returns one `Alignment`. The output is sorted by `score`, ascending.

```haskell
fuzzyFind :: [String] -> [String] -> [Alignment]
```

For example:

```
> fuzzyFind ["dad", "mac", "dam"] ["tinned macadamia"]
[Alignment {score = 296, result = Result [Gap "tinne", Match "d", Gap " ", Match "macadam", Gap "ia"]}]
```

