# Things you should avoid

Disclaimer: don't take it too seriously.

Some things are to be avoided. Let's look at a few examples in the Haskell land. Since there are going to be a few of them, it's natural to introduce a type class:

```haskell
-- | Things to be avoided.
class Avoid a where
    avoid :: a -> void
```

If you don't know what `void` means, check out [To Void or to void](https://tech.fpcomplete.com/blog/2017/07/to-void-or-to-void).

One thing that you definitely should avoid is any kind of false. For one example:

```haskell
data False

instance Avoid False where
    avoid = \case{}
```

For another:

```haskell
data AnythingIsTrue = AnythingIsTrue (forall a. a)
instance Avoid AnythingIsTrue where
    avoid (AnythingIsTrue void) = void
```

Don't be fooled by the [marketing](http://inutile.club/estatis/falso)!

Infinite loops are to be avoided as well. Both type-level:

```haskell
newtype TypeLevelLoop = TypeLevelLoop TypeLevelLoop

instance Avoid TypeLevelLoop where
    avoid (TypeLevelLoop loop) = avoid loop
```

and term-level ones:

```haskell
data TermLevelLoop = TermLevelLoop

instance Avoid TermLevelLoop where
    avoid = avoid
```

Exceptions? Avoid!

```haskell
data Exception = Exception

instance Avoid Exception where
    avoid Exception = error "You should have avoided this"
```

There's no place for incomplete implementations:

```haskell
data IncompleteImplementation = IncompleteImplementation

instance Avoid IncompleteImplementation
```

Avoid any kind of unsafety. For one example, `unsafeCoerce`. Or at least don't make it worse with overlapping instances:

```haskell
instance {-# OVERLAPPABLE #-} Avoid a where
    avoid = unsafeCoerce
```

For another example, `unsafePerformIO`:

```haskell
data UnsafePerformIO = UnsafePerformIO

instance Avoid UnsafePerformIO where
    avoid UnsafePerformIO =
        unsafePerformIO . forever $ putStrLn "I will not use 'unsafePerformIO' anymore"
```

False promises are not to be tolerated:

```haskell
newtype FalsePromise a = FalsePromise (Avoid a => a)
instance Avoid (FalsePromise a) where
    avoid (FalsePromise x) = avoid x
```

Finally, every success has a cost

```haskell
newtype Success cost = Success cost
```

so most importantly, avoid success at all costs!

```haskell
newtype AtAllCosts f = AtAllCosts
    { unAtAllCosts  :: forall cost. f cost
    }

instance Avoid (AtAllCosts Success) where
    avoid (AtAllCosts (Success void)) = void
```
