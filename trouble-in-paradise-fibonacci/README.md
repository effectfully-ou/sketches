# Trouble in paradise: Fibonacci

## Intro

Haskell is a lazy language, which has its consequences. In this post I'll demonstrate just how easy it is to shoot yourself in the foot with laziness using the classic example of computing Fibonacci numbers, which very ironically virtually all relevant Haskell documentation gets wrong, given that it's [the primary use case for Haskell](https://serokell.io/blog/compile-time-evaluation-haskell).

**DISCLAIMER**: this post is not meant to criticize Haskell for being lazy, I'll merely show how the language gets misused. I'm also not the first one to realize that the classic definitions of the Fibonacci sequence are problematic (someone on the Internet pointed that out to me, but that was long ago and I don't remember who that was). And this post is not about computing Fibonacci numbers efficiently -- it's about not leaking space with the Fibonacci sequence taken as an example.

## The problem

Full code is in [`src/Main.hs`](src/Main.hs).

When I first saw how to define a function retrieving the nth Fibonacci number from a neat self-referential Fibonacci sequence

```haskell
fibs_zw :: [Integer]
fibs_zw = 0 : 1 : zipWith (+) fibs_zw (tail fibs_zw)

fib_zw :: Int -> Integer
fib_zw n = fibs_zw !! n
```

I was amused by how elegant it was. I still think it's very elegant, but here's a crazy idea: let's run that code!

We're going to look at several different implementations, so let's first set up an executable that takes a command-line mode argument and chooses a particular implementation depending on the mode:

```haskell
main :: IO ()
main = do
    [mode] <- getArgs
    let run = print . length . show
    case mode of
        -- To be expanded.
```

Our first implementation is `fib_zw` from the above, so we add the following line to `main`:

```haskell
        "zw" -> run $ fib_zw 500000
```

With these GHC flags enabled: `-O2 -rtsopts "-with-rtsopts=-sstderr"` we do `stack build` and run

```
stack exec trouble-in-paradise-fibonacci zw
```

which results in

```
104494
  11,066,303,736 bytes allocated in the heap
   2,606,041,608 bytes copied during GC
      28,442,304 bytes maximum residency (1620 sample(s))
       4,256,536 bytes maximum slop
              27 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      8799 colls,     0 par    0.292s   0.347s     0.0000s    0.0019s
  Gen  1      1620 colls,     0 par   10.791s  10.887s     0.0067s    0.0773s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    2.086s  (  2.112s elapsed)
  GC      time   11.083s  ( 11.233s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time   13.170s  ( 13.345s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    5,304,191,152 bytes per MUT second

  Productivity  15.8% of total user, 15.8% of total elapsed
```

27 MB peak memory usage. Is 500000th Fibonacci number that big? Nope, only 15.8% of time is spent on the actual computation, i.e. a whopping 84.2% of time is spent during garbage collection. That's how a really nasty space leak looks like, ladies and gentlemen.

Here's what's going on. `zipWith` is defined in `base` like that:

```haskell
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f = go
  where
    go [] _ = []
    go _ [] = []
    go (x:xs) (y:ys) = f x y : go xs ys
```

I.e. it traverses both its list arguments lazily and produces a list lazily as well, where all elements are of the `f x y` form. If a list produced by `zipWith` is consumed by a function that forces the spine of the list, but does not force its elements, then those `f x y` are not going to be evaluated by that consumer at all. For example, `length` does not disturb the elements of a list, only its spine, and so the elements created by `zipWith` will not be evaluated, if the consumer is `length`:

```
>>> length $ zipWith (\_ _ -> error "boom") "abc" "defg"
3
```

Similarly, `xs !! n` does not evaluate any of the elements of `xs`, it only returns whatever `xs` has at the `n`th position. I.e. if `xs` has a thunk as its `n`th element, then `xs !! n` returns that thunk, which may or may not be forced down the pipeline, depending on the pipeline.

So `zipWith` being lazy in elements that it produces makes self-referential `fibs_zw` expand to

```
[0, 1, 0 + 1, 1 + (0 + 1), (0 + 1) + (1 + (0 + 1)), ...]
```

and `(!!)` does not force any intermediate results, meaning that `fibs_zw !! n` builds up a huge thunk and returns it and only then the thunk gets forced by the caller (`run`).

## Solutions

### Solution 1

One solution is to make `(!!)` strict:

```haskell
at' :: Int -> [a] -> a
at' n0 | n0 < 0 = error "bad index"
at' n0 = go n0 where
    go _ []         = error "not enough elements"
    go 0 (x:_)      = x
    go n ((!_): xs) = go (n - 1) xs  -- [1]

fib_zw1' :: Int -> Integer
fib_zw1' n = at' n fibs_zw
```

Now thunks get evaluated in `[1]` (due to the bang) as the list gets processed. Note that we use the same `fibs_zw` that was leaking with `(!!)` but no longer leaks with `at'`, which we can check by adding

```haskell
        "zw1s" -> run $ fib_zw1' 500000
```

to `main` and running

```
stack exec trouble-in-paradise-fibonacci zw1s
```

which gives us

```
104494
  11,045,529,328 bytes allocated in the heap
       2,535,400 bytes copied during GC
          44,448 bytes maximum residency (2 sample(s))
          29,280 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10356 colls,     0 par    0.034s   0.034s     0.0000s    0.0004s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.856s  (  0.881s elapsed)
  GC      time    0.034s  (  0.034s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.890s  (  0.915s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    12,911,046,426 bytes per MUT second

  Productivity  96.1% of total user, 96.3% of total elapsed
```

0 MB total memory in use and only 3.9% of time is spent on GC.

By the way, I had to write `((!_):xs)`, because apparently `go n (!_:xs)` is parsed as `go n (!(_:xs))`, which does not force the head element (the bang does not do anything in this case at all).

### Solution 2

Another option is to define the Fibonacci sequence in terms of a strict version of `zipWith` forcing list elements before returning them. For that we'll need a strict version of `(:)`:

```haskell
infix 4 `cons'`
cons' :: a -> [a] -> [a]
cons' !x xs = x : xs
```

The strict `zipWith` is this then:

```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = go where
    go (x : xs) (y : ys) = f x y `cons'` go xs ys
    go _        _        = []
```

Note that `zipWith'` produces forced list elements, but it still does so lazily, so if the consumer is, say, `length . take 3`, then only the first three elements will be evaluated (with the non-strict `zipWith` no elements would be evaluated).

So we can get a non-leaking version of `fibs_zw` only by replacing `zipWith` with `zipWith'`:

```haskell
fibs_zw' :: [Integer]
fibs_zw' = 0 : 1 : zipWith' (+) fibs_zw (tail fibs_zw)

fib_zw2' :: Int -> Integer
fib_zw2' n = fibs_zw' !! n
```

In this version we force list elements in the producer (`zipWith'`) rather than the consumer like in the previous version (`at'`).

### Solution 3

But instead of redefining existing functions we can plug in the middle of the pipeline a function that "entangles" the elements of a list with its spine, so that whenever the outermost constructor gets forced, the head element also gets forced (unless the list is empty, in which case nothing extra happens). For that we only need to rebuild the list replacing each `(:)` with its strict version:

```haskell
forceElems :: [a] -> [a]
forceElems = foldr cons' []
```

And here's how we can use `forceElems` to define another non-leaking function computing the `n`th Fibonacci number:

```haskell
fib_zwfe :: Int -> Integer
fib_zwfe n = forceElems fibs_zw !! n
```

The stats:

```
104494
  11,069,530,256 bytes allocated in the heap
       2,664,656 bytes copied during GC
          44,448 bytes maximum residency (2 sample(s))
          29,280 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     10358 colls,     0 par    0.032s   0.031s     0.0000s    0.0004s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.843s  (  0.852s elapsed)
  GC      time    0.032s  (  0.031s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.875s  (  0.884s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    13,133,000,414 bytes per MUT second

  Productivity  96.3% of total user, 96.5% of total elapsed
```

### Solution 4

Finally, we can use a completely different definition of the Fibonacci sequence like this one:

```haskell
fibs_sl' :: [Integer]
fibs_sl' = 0 : scanl' (+) 1 fibs_sl'
```

This also does not leak. It's the second most popular definition of the Fibonacci sequence, but usually instead of `scanl'` the leaky `scanl` is used (see e.g. [Haskell wiki](https://wiki.haskell.org/The_Fibonacci_sequence)), which makes the whole thing leak. `scanl`/`foldl` not being strict is [such a stupid default](https://www.well-typed.com/blog/2014/04/fixing-foldl).

## Better testing

Seeing stats is nice, but can we catch these space leak problems in actual tests? Yes! Enter `nothunks` (the [library](https://hackage.haskell.org/package/nothunks), the [explanation](https://www.well-typed.com/blog/2020/09/nothunks)). `nothunks` allows us to check if a value has any thunks in it. Consequently, we can turn a value containing thunks into an error:

```haskell
throwOnThunks :: NoThunks a => a -> a
throwOnThunks x = case unsafeNoThunks x of
    Nothing -> x
    Just _  -> error "A thunk!"
```

which allows us to turn every list element having thunks in it into an error:

```haskell
thunkElemsToErrorsLazy :: NoThunks a => [a] -> [a]
thunkElemsToErrorsLazy = map throwOnThunks
```

Note that we do not have `thunkElemsToErrorsLazy = throwOnThunks` even though there's a `NoThunks` instance for `[a]`. This is because the spine of the list is allowed to be thunkified -- only elements aren't (in fact, there's no way we could have an infinite list with its spine fully forced as that would take an infinite amount of memory).

However `thunkElemsToErrorsLazy` is way too lazy: collecting a huge thunk of added up error calls up is no different to collecting a huge thunk of added up integers in terms of memory consumption, so even though we will get an error in the end if we call `thunkElemsToErrorsLazy` over a leaking version of the Fibonacci sequence, going through all the unnecessary GC would be wasteful (in practice GHC is able to realize there's an error and fail somewhere in the middle of the computation). So we need to trigger the error as early as possible, i.e. when the spine of the list gets disturbed. And we already know how to entangle the elements of a list with its spine:

```haskell
thunkElemsToErrors :: NoThunks a => [a] -> [a]
thunkElemsToErrors = forceElems . thunkElemsToErrorsLazy
```

And now we can check that running a leaking version of the Fibonacci sequence

```haskell
unsafeFib_zw :: Int -> Integer
unsafeFib_zw n = thunkElemsToErrors fibs_zw !! n
```

does give us an error immediately:

```
trouble-in-paradise-fibonacci: A thunk!
CallStack (from HasCallStack):
  error, called at src/Main.hs:62:16 in main:Main
          95,088 bytes allocated in the heap
           4,712 bytes copied during GC
          37,584 bytes maximum residency (1 sample(s))
          19,760 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         0 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.000s  (  0.000s elapsed)
  GC      time    0.000s  (  0.000s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.000s  (  0.000s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    0 bytes per MUT second

  Productivity 100.0% of total user, 48.3% of total elapsed
```

and running a non-leaking version of the Fibonacci sequence

```haskell
unsafeFib_zw1' :: Int -> Integer
unsafeFib_zw1' n = thunkElemsToErrors fibs_zw' !! n
```

gives us the correct result:

```
104494
  26,572,686,816 bytes allocated in the heap
       5,958,056 bytes copied during GC
          44,448 bytes maximum residency (2 sample(s))
          29,280 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0     21053 colls,     0 par    0.065s   0.063s     0.0000s    0.0000s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    2.235s  (  2.274s elapsed)
  GC      time    0.065s  (  0.064s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    2.300s  (  2.338s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    11,889,286,816 bytes per MUT second

  Productivity  97.2% of total user, 97.3% of total elapsed
```


## Still problematic

Okay, now that we've done everything to ensure that `fibs_zw'` does not leak space, are we safe to use `fib_zw2'` defined in terms of it as

```
fib_zw2' :: Int -> Integer
fib_zw2' n = fibs_zw' !! n
```

? Here's another test case: we add the following to `main`:

```
        "zw2s_2" -> do
            run $ fib_zw2' 100000
            run $ fib_zw2' 99999
```

(note that `100000` is five times smaller than `500000` that we were using before), execute

```
stack exec trouble-in-paradise-fibonacci zw2s_2
```

and get

```
20899
20899
     905,348,904 bytes allocated in the heap
     409,019,416 bytes copied during GC
     273,760,824 bytes maximum residency (11 sample(s))
     112,754,120 bytes maximum slop
             261 MB total memory in use (1 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       884 colls,     0 par    0.015s   0.027s     0.0000s    0.0004s
  Gen  1        11 colls,     0 par    0.086s   0.178s     0.0162s    0.0346s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.065s  (  0.129s elapsed)
  GC      time    0.100s  (  0.205s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.165s  (  0.334s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    13,912,178,130 bytes per MUT second

  Productivity  39.3% of total user, 38.7% of total elapsed
```

261 MB total memory in use! For computing much smaller integers than the ones we were dealing with before!

Well, yes, `fibs_zw'` is a [CAF](https://wiki.haskell.org/Constant_applicative_form) and GHC sees that it's used twice, hence all list elements that were computed during the first traversal (`fib_zw2' 100000`) stay in memory, 'cause the second traversal (`fib_zw2' 99999`) might need them and recomputing things in the general case would be wasteful. "But recomputing them in this case instead of keeping them in memory would make perfect sense!" -- yeah, it would, but how could GHC tell when it's worth to recompute something and when it's worth to keep it in memory? GHC is [call-by-need](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_need), has [monomorphism restriction](https://wiki.haskell.org/Monomorphism_restriction) and so on, meaning GHC does try hard to avoid needless recomputation.

In this case it's not a big deal to spot what the problem is, but with [full laziness](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-optimisation.html#ghc-flag--ffull-laziness) turned on (which is the default) GHC can produce some [quite intricate space leaks](https://www.well-typed.com/blog/2016/09/sharing-conduit).

What to do then? We could just use recursion instead of all this caching of intermediate results:

```haskell
fib_loop :: Int -> Integer
fib_loop n0 = go n0 0 1 where
    go 0 curr _    = curr
    go n curr next = go (n - 1) next $! curr + next
```

But we do want to have caching if instead of retrieving the `n`th element of the sequence, we need to, say, add up the first `n` elements. In that latter case `sum . take n $ map fib_loop [0..]` would pointlessly recompute all the same intermediate results over and over again.

So yeah, by introducing caching you risk to blow up memory, by not introducing it you risk to recompute things too much. Life is like that.

## Conclusions

- Laziness can be a pain in the ass, but you already knew that.
- Haskell docs often fail to elaborate on issues related to operational semantics (how things evaluate) as opposed to denotational semantics (what they evaluate to).
- [`nothunks`](https://hackage.haskell.org/package/nothunks) is amazing.
- You need to be careful with CAFs or anything that can potentially become one.

If you liked this post and appreciate the effort, consider becoming a [sponsor](https://github.com/sponsors/effectfully-ou) (starts from 1$).
