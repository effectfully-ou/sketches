# A custom warning hack

This post describes how to hackily set up a helpful warning where you normally wouldn't get it. Full code is in [`src/Main.hs`](./src/Main.hs).

Consider the following data type:

```haskell
data D
  = C0
  | C1 Int Bool
  | C3 Char
```

You might want to parse it:

```haskell
parseC0 :: Parse D
parseC1 :: Parse D
parseC3 :: Parse D

parseD :: Parse D
parseD = parseC0 <|> parseC1 <|> parseC3
```

That works alright, but extending `D` with an additional constructor, say,

```haskell
  | C2 Word
```

won't trigger any warning about `parseD` not handling this new constructor. Which can be troubling, especially when someone contributes to your codebase and doesn't even know there's a parser to extend. And the same applies to deserialization, decoding from JSON etc. So that problem does appear in the wild. Tests should normally catch it, but proper tests are not always there and staring at failed tests and trying to make sense of what just happened is more expensive than preventing them from failing in the first place.

Ideally, it would be nice to have some kind of cocoverage checking, so that we could write something like

```haskell
parseD :: Parse Dn
C0{} <- parseD = parseC0
C1{} <- parseD = parseC1
C3{} <- parseD = parseC3
```

meaning "try `parseC0` and if that fails or doesn't return a `C0`, then try `parseC1` etc", which would allow the compiler to check that all the constructors are mentioned to the left of the `->`s.

But we don't have any of that and so we can simply introduce a separate function matching on all the constructors of the data type and turning the regular incomplete patterns warning into something a bit more suggestive:

```haskell

class Warning warning where
  warning :: warning -> ()

data FIX_ME_BUT_FIRST request a
  = LOOK_RIGHT_ABOVE
  | FIX_ME_BUT_FIRST request a

data IMPLEMENT_PARSING_FOR = IMPLEMENT_PARSING_FOR

instance Warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR D) where
  warning LOOK_RIGHT_ABOVE                              = ()
  warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR C0{}) = ()
  warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR C1{}) = ()
  warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR C3{}) = ()
```

Now adding the `C2` constructor to `D` gives us the following warning when type checking `warning`:

```
.../custom-warning-hack/src/Main.hs:34:3: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘warning’:
        Patterns not matched: FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR (C2 _)
   |
34 |   warning LOOK_RIGHT_ABOVE                                     = ()
   |   ^
```

Making `warning` a method of a type class ensures that you won't get an unused function warning and it's also nice to pollute the global namespace with a bit fewer redundant names and instead share the same name for all functions triggering custom warnings.

Overall, far from perfect, but better than nothing.

**UPDATE** A number of people have [commented](https://www.reddit.com/r/haskell/comments/pi4hl3/a_custom_warning_hack) on this post suggesting alternative ideas:

1. instead of using a type class method, one can simply have a local function whose name starts with `_` and a comment above it requesting to update something. I think I still prefer for some kind of suggestion to appear in the warning itself, but YMMV
2. there exist packages solving this exact problem (differently): [`surjective`](https://hackage.haskell.org/package/surjective), [`exhaustive`](https://hackage.haskell.org/package/exhaustive), [`sum-totality`](https://github.com/aaronallen8455/sum-totality)
