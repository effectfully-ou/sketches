# validation-leak

`Validation` and its applicative instance are defined something like this:

```haskell
data Validation e a
  = Failure e
  | Success a

instance Semigroup e => Applicative (Validation e) where
  pure = Success

  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e  <*> Success _  = Failure  e
  Success _  <*> Failure e  = Failure  e
  Success f  <*> Success x  = Success (f x)
```

Look at these two lines:

```haskell
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e  <*> Success _  = Failure  e
```

If the first argument to `(<*>)` is a `Failure`, then we have a `Failure` regardless of what the second argument is.
But nevertheless the second argument is forced before a final `Failure` is returned. This means that before
any actual errors are returned, a whole computation must finish -- no lazy streaming of errors is possible.
Which also implies that `Validation` can never be short-circuiting: even if you don't care about actual errors
and just want to know whether a computation has finished successfully, you won't be able to stop early once an error has occured.
In the same way if you only care about some particular error and have encountered it, it won't be possible to stop here --
all errors must be collected before the result of a `Validation e a` computation can be handled in any way.

Here is a simple fix:

```haskell
instance Monoid e => Applicative (Validation e) where
  pure = Success

  Failure e1 <*> b = Failure $ e1 `mappend` case b of
    Failure e2 -> e2
    Success _  -> mempty
  Success _  <*> Failure e  = Failure  e
  Success f  <*> Success x  = Success (f x)
```

If the first argument to `(<*>)` is a `Failure e1`, then return a `Failure` and prepend `e1` to the result which can be either `mempty`
(if there are no more errors) or some `e2`. Note that this requires `e` to be a `Monoid` while previously it only had to be a `Semigroup`,
so this instance is not strictly better than the leaking one.

Similar considerations apply to `Control.Applicative.Lift`:

```haskell
instance Applicative f => Applicative (Lift f) where
  pure = Pure

  Pure  f <*> Pure x  = Pure  (f x)
  Pure  f <*> Other y = Other (f <$> y)
  Other f <*> Pure x  = Other (($ x) <$> f)
  Other f <*> Other y = Other (f <*> y)
```

Though, `($ x) <$> f` is better than `f <*> pure x`, so it's not immediately clear whether the `Applicative` instance for `Lift` should be changed.

## Testing

A simple test

```haskell
multifail :: (Except f [Int], Applicative f) => f [Int]
multifail = go 1000000 where
  go 0 = pure []
  go n = throw [n] *> go (n - 1)
```

reveals that `Leak.Validation` indeed leaks:

```
500000500000
      72,666,928 bytes allocated in the heap
     125,238,824 bytes copied during GC
      36,110,016 bytes maximum residency (7 sample(s))
       6,854,976 bytes maximum slop
              87 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       132 colls,     0 par    0.040s   0.048s     0.0004s    0.0009s
  Gen  1         7 colls,     0 par    0.044s   0.055s     0.0078s    0.0275s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.016s  (  0.017s elapsed)
  GC      time    0.084s  (  0.103s elapsed)
  EXIT    time    0.000s  (  0.004s elapsed)
  Total   time    0.124s  (  0.124s elapsed)

  %GC     time      67.7%  (83.1% elapsed)

  Alloc rate    4,541,683,000 bytes per MUT second

  Productivity  32.3% of total user, 16.7% of total elapsed
```

and `Fine.Validation` indeed doesn't leak:

```
500000500000
      80,053,448 bytes allocated in the heap
         270,200 bytes copied during GC
          44,384 bytes maximum residency (2 sample(s))
          53,352 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       152 colls,     0 par    0.000s   0.000s     0.0000s    0.0002s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.008s  (  0.009s elapsed)
  GC      time    0.000s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.032s  (  0.010s elapsed)

  %GC     time       0.0%  (6.0% elapsed)

  Alloc rate    10,006,681,000 bytes per MUT second

  Productivity 100.0% of total user, 92.4% of total elapsed
```
