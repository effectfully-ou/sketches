# A law-breaking hack

I saw a simple question, something like

> How do you zip all elements in a list with `True` apart from the last one which you zip with `False`?

and so I naturally asked myself "what would be the silliest way of doing this?". This post is an answer: I'll show how to abuse laziness and what kind of horrible consequences that has (spoiler: you can break a law by being lazy).

Ok, so it would certainly be quite silly if we literally used `zip` over an input list and an arbitrary-length list of `True`s always ending in `False`. I.e. if we had such `lastIsFalse :: [Bool]` that

```
take (n + 1) lastIsFalse === replicate n True ++ [False]
```

holds for any `n`. Can we have such a definition in Haskell? Certainly not purely, but with ~undefined behavior~ `unsafePerformIO` anything is possible. Here we go:

```haskell
lastIsFalse :: [Bool]
lastIsFalse = unsafePerformIO $ do
    next <- newIORef False  -- [1]
    let go = do
                b <- unsafeInterleaveIO $ readIORef next  -- [2]
                bs <- unsafeInterleaveIO $ do             -- [3]
                    writeIORef next True                  -- [4]
                    _ <- evaluate b                       -- [5]
                    writeIORef next False                 -- [6]
                    go                                    -- [7]
                pure $ b : bs                             -- [8]
    go
```

Step by step:

- [1]: the next element of a list is going to be stored in a variable called `next`
- [2]: lazily retrieve the next element of a list. We don't know if it's `False` or `True` yet, 'cause `unsafeInterleaveIO` defers the `IO` action until its result is forced (we could've alternatively written `let b = unsafePerformIO $ readIORef next`, which is also lazy due to `let`-bindings being evaluated lazily) . At this point we only know that the value for `b` is going to be read from `next` when `b` is demanded
- [3]: lazily retrieve the tail of the list and whenever it's forced by the caller, do all of these:
  * [4] & [5]: put `True` into `next` and immediately afterwards evaluate `b`. This ensures that `b` is `True` whenever the tail of the list is forced. I.e. if the caller asks for more elements, then `b` is not the last one that the caller needs and so it has to be `True`
  * [6] & [7]: put `False` back into `next`, so that if the tail of the tail is never forced, then the head of the tail will be read from `next` as `False`. Then recurse
- [8]: attach the head to the tail

We can check that this works as expected for a single-element list:

```
>>> init . take 1 $ zip [1..] lastIsFalse
[]
>>> last . take 1 $ zip [1..] lastIsFalse
(1,False)
```

a two-element list:

```
>>> init . take 2 $ zip [1..] lastIsFalse
[(1,True)]
>>> last . take 2 $ zip [1..] lastIsFalse
(2,False)
```

and, say, a five-element list:

```
>>> init . take 5 $ zip [1..] lastIsFalse
[(1,True),(2,True),(3,True),(4,True)]
>>> last . take 5 $ zip [1..] lastIsFalse
(5,False)
```

"Okay, that seems to work, but why do you ask for the `init` and `last` of a list separately?" -- here's why:

```
>>> take 5 $ zip [1..] lastIsFalse
[(1,False),(2,False),(3,False),(4,False),(5,False)]
```

Whoops, that's not right. Here's what's happening. Normally both `init` and `last` force the spine of a list and don't force any elements. But the spine of `lastIsFalse` is "entangled" with its elements, so in our case whenever the tail of a sublist of `lastIsFalse` is forced, the head of that sublist immediately evaluates to `True` and as such:

- `init` forces all elements it returns and the remaining one never gets forced
- `last` forces all elements it drops and the last one gets forced only when it's about to be printed

But if we have neither `init` nor `last`, then the spine does not get forced before the elements do. Instead, the implicit `show` forces each element before forcing the rest of the list, i.e. before our carefully crafted entanglement has a chance to kick in and determine that the element at hand is not the last one.

Hence we have to explicitly force the spine to get the right result. That can be done either via a strict right fold (never heard of that one, did you?):

```haskell
foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' f = foldr (($!) . f)
```

like this:

```
>>> foldr' (:) [] . take 5 $ zip [1..] lastIsFalse
[(1,True),(2,True),(3,True),(4,True),(5,False)]
```

or, more interestingly, via `traverse pure`:

```
>>> traverse pure . take 5 $ zip [1..] lastIsFalse
[(1,True),(2,True),(3,True),(4,True),(5,False)]
```

which forces the spine of the list just like `foldr'` due to `IO` being a strict `Applicative`, which causes `traverse` to process the entire list before anything can be returned (see [Constructing a list in a Monad](https://www.joachim-breitner.de/blog/620-Constructing_a_list_in_a_Monad) for a detailed analysis).

... but according to [the `Traversable` laws](https://en.wikibooks.org/wiki/Haskell/Traversable#The_Traversable_laws) `traverse pure` is supposed to be equal to `pure`. We can check that this is not the case:

```
>>> pure . take 5 $ zip [1..] lastIsFalse
[(1,False),(2,False),(3,False),(4,False),(5,False)]
```

Moral of the story: if there's laziness exposed to the user, all laws are off. Which is of course something that people are well aware of:

- [State monads don't respect the monad laws in Haskell](https://mail.haskell.org/pipermail/haskell/2002-May/009622.html)
- [Hask is not a category](http://math.andrej.com/2016/08/06/hask-is-not-a-category/)

`lastIsFalse` is particularly bad, because its value depends on how that value is consumed. Ah, and yeah, you can only step in the same river once:

```
>>> traverse pure . take 3 $ zip [1..] lastIsFalse
[(1,True),(2,True),(3,False)]
>>> traverse pure . take 5 $ zip [1..] lastIsFalse
[(1,True),(2,True),(3,False),(4,True),(5,False)]
```

(note the `(3,False)` in the final line). I.e. if a part of `lastIsFalse` is forced at some point, it's going to stay the same if `lastIsFalse` is used again in the same program or GHCi session (all previous examples were separate GHCi sessions) due to`lastIsFalse` being a [CAF](https://stackoverflow.com/questions/8330756/what-are-super-combinators-and-constant-applicative-forms).

Don't do any of that at home.

If you liked this post and appreciate the effort, consider becoming a [sponsor](https://github.com/sponsors/effectfully-ou) (starts from 1$).
