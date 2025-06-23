# Better counterexample minimization

When it comes to getting a minimal counterexample using property-based testing (PBT), there's a significant flaw in the core design of QuickCheck, as well as PBT libraries in other ecosystems (unless ChatGPT lies to me about those, I don't care enough to check).

In QuickCheck if a property test fails, the inputs get progressively reduced until a minimal set is found -- this process is called shrinking. The function returning all one-step-reduced derivatives of an input is called `shrink` and has the following signature:

```haskell
shrink :: Arbitrary a => a -> [a]
```

If a test fails on `x`, then QuickCheck runs the test on the elements of `shrink x` until either a smaller derivative of `x` failing the test is found or all elements are tried (in which case `x` is considered minimal, unless we also shrink some `y` and come back to `x` later to try to shrink it again). Smallest shrinks are supposed to come first in the list returned by `shrink`.

The issue with `shrink` is that it's not only an interface (that part is alright), but also the function used internally to drive counterexample minimization. It is not enough to order shrinks smallest-first: that works fine when you only need to reduce one input, but when it comes to reducing multiple inputs it's usually best to try to reduce each one of them to the smallest value first, then each one of them to the next smallest value etc.

In other words, QuickCheck's `shrink` ends up performing a depth-first search in the space of shrunk values, whereas what we really want is a breadth-first search instead.

This is particularly troublesome when property-based testing programming languages. An AST is a tree and you always want to remove from it as much as possible before you start to e.g. perform beta-reductions to reach a minimal counterexample, as beta-reductions can duplicate parts of the AST.

To see how QuickCheck misbehaves when shrinking counterexamples we can pick any tree-like data type, for example this one:

```haskell
data Tree a
    = Nil
    | Branch a (Tree a) (Tree a)
```

This is how QuickCheck [recommends](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#v:shrink) defining `shrink` for `Tree`:

```haskell
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = undefined

    shrink :: Tree a -> [Tree a]
    shrink Nil = []
    shrink (Branch x l r) =
      -- shrink Branch to Nil
      [Nil] ++
      -- shrink to subterms
      [l, r] ++
      -- recursively shrink subterms
      [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]
```

If we actually check what this returns, we'll find that it returns plenty of duplicates:

```haskell
>>> import Test.QuickCheck
>>> mapM_ print . shrink $ Branch True (Branch True Nil Nil) (Branch True Nil Nil)
Nil
Branch True Nil Nil
Branch True Nil Nil
Branch False (Branch True Nil Nil) (Branch True Nil Nil)
Branch True Nil (Branch True Nil Nil)
Branch True Nil (Branch True Nil Nil)
Branch True Nil (Branch True Nil Nil)
Branch True (Branch False Nil Nil) (Branch True Nil Nil)
Branch True (Branch True Nil Nil) Nil
Branch True (Branch True Nil Nil) Nil
Branch True (Branch True Nil Nil) Nil
Branch True (Branch True Nil Nil) (Branch False Nil Nil)
```

The reason for it is that we shrink to `Nil`, `l` and `r`, but `l` and `r` may themselves be `Nil`, which is how we end up having duplicates. Fixing that is easy:

```haskell
    shrink :: Tree a -> [Tree a]
    shrink Nil = []
    shrink (Branch x l r) =
      -- shrink Branch to Nil
      [Nil] ++
      -- shrink to subterms
      [t | t@Branch{} <- [l, r]] ++
      -- recursively shrink subterms
      [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]
```

Now `shrink` doesn't produce duplicates:

```haskell
>>> import Test.QuickCheck
>>> mapM_ print . shrink $ Branch True (Branch True Nil Nil) (Branch True Nil Nil)
Nil
Branch True Nil Nil
Branch True Nil Nil
Branch False (Branch True Nil Nil) (Branch True Nil Nil)
Branch True Nil (Branch True Nil Nil)
Branch True (Branch False Nil Nil) (Branch True Nil Nil)
Branch True (Branch True Nil Nil) Nil
Branch True (Branch True Nil Nil) (Branch False Nil Nil)
```

There's still the main issue however: we produce the longer

```haskell
Branch False (Branch True Nil Nil) (Branch True Nil Nil)
```

before the shorter

```haskell
Branch True Nil (Branch True Nil Nil)
Branch True (Branch True Nil Nil) Nil
```

which violates the idea specified right next to the original definition of `shrink` for `Tree` in QuickCheck docs:

> QuickCheck tries the shrinking candidates in the order they appear in the list, so we put more aggressive shrinking steps (such as replacing the whole tree by `Nil`) before smaller ones (such as recursively shrinking the subtrees).

What we actually need to do is to shrink the spine of the given tree and its elements separately -- and put the former before the latter at the very last step. Here's how that can be implemented:

```haskell
    shrink :: Tree a -> [Tree a]
    shrink = uncurry (++) . go where
        go :: Tree a -> ([Tree a], [Tree a])
        go Nil = ([], [])
        go (Branch x l r) = (tSp, tEl) where
            (lSp, lEl) = go l
            (rSp, rEl) = go r
            tSp = concat
                [ [Nil]
                , [t | t@Branch{} <- [l, r]]
                , [Branch x l' r | l' <- lSp]
                , [Branch x l r' | r' <- rSp]
                ]
            tEl = concat
                [ [Branch x' l r | x' <- shrink x]
                , [Branch x l' r | l' <- lEl]
                , [Branch x l r' | r' <- rEl]
                ]
```

This isn't an efficient way of doing it (I made a [challenge](https://www.reddit.com/r/haskell/comments/1li3urz/optimize_a_tree_traversal) out of this optimization problem), but it'll do the job.

`shrink` implemented this way produces smallest trees first as expected:

```haskell
>>> import Test.QuickCheck
>>> mapM_ print . shrink $ Branch True (Branch True Nil Nil) (Branch True Nil Nil)
Nil
Branch True Nil Nil
Branch True Nil Nil
Branch True Nil (Branch True Nil Nil)
Branch True (Branch True Nil Nil) Nil
Branch False (Branch True Nil Nil) (Branch True Nil Nil)
Branch True (Branch False Nil Nil) (Branch True Nil Nil)
Branch True (Branch True Nil Nil) (Branch False Nil Nil)
```

Therefore we've solved the problem. Unfortunately, not fully. `shrink` now works well when tree elements are `Bool`s, but here's what happens if they're trees themselves:

```haskell
>>> import Test.QuickCheck
>>> mapM_ print . shrink $ Branch (Branch True Nil Nil) Nil (Branch (Branch True Nil Nil) Nil Nil)
Nil
Branch (Branch True Nil Nil) Nil Nil
Branch (Branch True Nil Nil) Nil Nil
Branch Nil Nil (Branch (Branch True Nil Nil) Nil Nil)
Branch (Branch False Nil Nil) Nil (Branch (Branch True Nil Nil) Nil Nil)
Branch (Branch True Nil Nil) Nil (Branch Nil Nil Nil)
Branch (Branch True Nil Nil) Nil (Branch (Branch False Nil Nil) Nil Nil)
```

This is pretty much the same issue as before, this value:

```haskell
Branch (Branch False Nil Nil) Nil (Branch (Branch True Nil Nil) Nil Nil)
```

appears before this one:

```haskell
Branch (Branch True Nil Nil) Nil (Branch Nil Nil Nil)
```

even though the latter is clearly smaller than the former.

This isn't surprising, we can't do better than that when the instance is defined like this:

```haskell
instance Arbitrary a => Arbitrary (Tree a) where
    <...>
```

because we only have access to the general `shrink @a` and it's going to return all possible shrinks without any meta info on whether they're spine shrinks or element shrinks.

We can address it by either making a version of `Arbitrary` that has `shrink` associating meta info with each of returned elements, for example

```haskell
class Arbitrary a where
    arbitrary :: <...>
    shrink :: [a] -> [(Size, a)]
```

or by deeply propagating the shrink-spine-and-elements-in-parallel logic via a separate type class:

```haskell
class PriorityShrink a where
    priorityShrink :: a -> ([a], [a])

shrinkViaPriorityShrink :: PriorityShrink a => a -> [a]
shrinkViaPriorityShrink = uncurry (++) . priorityShrink

instance PriorityShrink a => PriorityShrink (Tree a) where
    -- Here goes @go :: Tree a -> ([Tree a], [Tree a])@ from the above.
    priorityShrink = <...>

instance PriorityShrink a => Arbitrary (Tree a) where
    arbitrary = <...>
    shrink = shrinkViaPriorityShrink
```

but then we also need to worry about having more than two priorities. For example, when shrinking an an integer `i` we could assign the highest priority to shrinking it to `0`, the next highest priority to shrinking it to `isqrt i`, the third highest priority to shrinking it to ``i `div` 2`` and so on.

So it is perhaps simplest to return a `Size` alongside every shrunk value, treat those `Size`s as weights in a graph and run something like the Dijkstra's algorithm to determine the optimal ordering of shrunk values.
