# And-patterns for exhaustive unordered pattern matching

Consider some arbitrary data type `R` with two `Int` fields:

```haskell
data R = R
    { a :: Int
    , b :: Int
    }
```

How would you implement, say, serialization for its values? You'd probably either do

```haskell
serializeR (R x y) = do
    serializeA x
    serializeB y
```

or, with `NamedFieldPuns` enabled (note that the order of the arguments doesn't matter):


```haskell
serializeR R{b, a} = do
    serializeA a
    serializeB b
```

Both of these approaches can be problematic:

1. for the former: what if somebody changes the ordering of the fields in `R` later on without realizing that `serializeR` needs to reflect the change? The type checker won't help in this case, since both the fields are of type `Int`
2. for the latter: what if somebody adds more fields to `R` later on without realizing that `serializeR` needs to reflect the change? The type checker won't help in this case, since the exhaustiveness of named-field-puns-matching isn't checked (there's a GHC [issue](https://gitlab.haskell.org/ghc/ghc/-/issues/15855) about that)

One way to address this problem is by matching on the `R` value twice: once to ensure exhaustiveness and once to get the actual arguments without caring about their order:

```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

dup :: a -> (a, a)
dup x = (x, x)

infixr 0 :&
pattern (:&) :: a -> a -> a
pattern p1 :& p2 <- (dup -> (p1, p2))
{-# COMPLETE (:&) #-}

serializeR :: R -> IO ()
serializeR (R _ _ :& R {b, a}) = do
    serializeA a
    serializeB b
```

**UPD**

u/rampion [suggested](https://www.reddit.com/r/haskell/comments/1d9kexm/comment/lbatjee/) to use this implementation instead:

```haskell
pattern (:&) :: a -> a -> a
pattern a0:&a1 <- a0@a1
{-# COMPLETE (:&) #-}
```

which magically works despite `@` not working directly without a pattern synonym. Go figure!

With this approach changing the order of fields in `R` will not change the semantics of `serializeR` and adding new fields to `R` will cause pattern matching to fail with an error message like

```
    • The constructor ‘R’ should have 3 arguments, but has been given 2
    • In the pattern: R _ _
      In the pattern: R _ _ :& R {b, a}
      In an equation for ‘serializeR’:
          serializeR (R _ _ :& R {b, a})
            = do serializeA a
                 serializeB b
```

hence both the issues are solved. The trade-offs are

1. the trick makes the code look cryptic
2. when adding new fields to `R` and fixing `serializeR` there's a danger of increasing the number of `_`s while forgetting to add the new arguments as named fields puns (the `{b, a}` part)
3. the helpfulness of the trick reduces with each argument added, imagine counting underscores when there are dozens of them. Perhaps that could be automated via a standlone tool though, but that's a hassle. Maybe it's worth it for mission-critical code

Also I wish `@` was an and-pattern instead of requiting its left operand to be a variable name, so that I could write this:

```haskell
serializeR :: R -> IO ()
serializeR (R _ _)@(R {b, a}) = do
    serializeA a
    serializeB b
```

**UPD**: this post was [discussed](https://www.reddit.com/r/haskell/comments/1d9kexm/andpatterns_for_exhaustive_unordered_pattern) on Reddit.

/u/brandonchinn178 [pointed](https://www.reddit.com/r/haskell/comments/1d9kexm/comment/l7ex96u/) out that [this](https://github.com/ghc-proposals/ghc-proposals/pull/436) proposal would address the issue on the language level.

u/Faucelme [pointed](https://www.reddit.com/r/haskell/comments/1d9kexm/comment/l7i5csk/) out that making `serializeR` linear would require consuming all of its fields.

u/enobayram [pointed](https://www.reddit.com/r/haskell/comments/1d9kexm/comment/l7is4v1/) out that the [`safe-wild-cards`](https://hackage.haskell.org/package/safe-wild-cards-1.0.0.2) package can be used instead of the trick described in this post.

u/Innf107 [pointed](https://www.reddit.com/r/haskell/comments/1d9kexm/comment/l7fsz8c/) out that the coverage checked hates the trick and it indeed does if you add another constructor with fields to the `R` data type.

Check out the full discussion for some more insightful comments.
