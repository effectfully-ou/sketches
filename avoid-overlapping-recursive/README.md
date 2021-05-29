# Avoiding overlapping instances in the recursive case

There is a [folklore trick](https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html) that allows to avoid overlapping instances in certain cases. There is also a [funny old puzzle](https://stackoverflow.com/questions/28003135/is-it-possible-to-encode-a-generic-lift-function-in-haskell) about arity-generic `liftA` function. This post shows how to adapt the former to solve the latter.

So the task is to implement a single function that generalizes all of those:

```haskell
liftA  :: Applicative f => (a -> b)           -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c)      -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
...
```

Intuitively, it's pretty clear how to handle that: just lift the `f :: a -> b -> ...` function to `ff :: f (a -> b -> ...)`, recurse over the `f a -> f b -> ...` type, strip each argument `x :: f x` one by one and use `<*>` to apply `ff` to every `x` until we run out of arguments. I.e.

```
liftA  f = \(a :: f a)                       -> pure f <*> a
liftA2 f = \(a :: f a) (b :: f b)            -> pure f <*> a <*> b
liftA3 f = \(a :: f a) (b :: f b) (c :: f c) -> pure f <*> a <*> b <*> c
...
```

The problem however is in the "until we run out of arguments" part. What we want to check is that there are no `->` left in the type, but the final `f z` may itself be `x -> z`, because `f` itself may be `(->) x`. This asks for overlapping instances and this is what people use in the Stack Overflow thread linked above (and there is a solution that uses `IncoherentInstances`).

But in fact, we don't actually need overlapping instances here. It's sufficient to pattern match on the type in a type family and return either `True` or `False` depending on whether the type is functional or not. And then dispatch on the result using a bespoke type class instead of dispatching on the original type. So we'll have two type classes: one for dispatcing and one with the actual function we're interested in (let's call the latter the interface class and its method -- the interface function). This is what the avoid-overlapping-instances trick mentioned in the beginning amounts to.

We need an additional twist however: in the recursive case (i.e. when the type is functional) in the dispatching instance we call the interface function, because once `ff` is applied to a `x :: f x`, we're back to the original state where we want to apply `ff` to a number (possibly zero) of `x :: f x`, so we just use the interface function, because it's what it's for. So the methods from the two type classes call each other in a mutually recursive fashion.

[The code](src/Main.hs) is pretty concise:

```haskell
type family IsFun a :: Bool where
    IsFun (_ -> _) = 'True
    IsFun  _       = 'False

class DispatchApN f a b (ts :: Bool) where
    dispatchApN :: Proxy# ts -> f a -> b

instance f a ~ b => DispatchApN f a b 'False where
    dispatchApN _ = id

instance (Applicative f, xa ~ (x -> a), fxb ~ (f x -> b), ApN f a b) =>
            DispatchApN f xa fxb 'True where
    dispatchApN _ f = \a -> apN $ f <*> a

class Applicative f => ApN f a b where
    apN :: f a -> b

instance (Applicative f, DispatchApN f a b (IsFun b)) => ApN f a b where
    apN = dispatchApN (proxy# :: Proxy# (IsFun b))

liftAn :: ApN f a b => (x -> a) -> f x -> b
liftAn f = apN . fmap f
```

This works in the polymorphic case, as well as for nested applicatives, e.g.

```haskell
test :: (Enum a, Num a) => Maybe (Maybe a)
test = (liftAn . liftAn) (+) (Just (Just 4)) (Just (Just 1))
```

That's it for the avoid-overlapping-instances-in-the-recursive-case trick. The rest of the post is about improvements of this solution and alternatives.

## Bonus: better type inference

The solution presented above doesn't handle a common use case that other approaches can handle: lifting a monomorphic function without providing a type signature for the whole application. E.g.

```haskell
test = liftAn (++) (Just "a") (Just "b")
```

gives an incomprehensible error:

```
    • Ambiguous type variable ‘t0’ arising from a use of ‘liftAn’
      prevents the constraint ‘(DispatchApN
                                  Maybe [Char] t0 (IsFun t0))’ from being solved.
```

Looking at

```haskell
class Applicative f => ApN f a b where
    apN :: f a -> b
```

what we want to express is that whenever `a` is non-functional, so is `b`. Note that the converse does not hold, because e.g. in

```haskell
test :: (Enum a, Num a) => [a]
test = liftAn (+) [1..5] [3..5]
```

the user is able to instantiate `a` with any type, including a functional one, and we don't want to sacrifice that (at the very least because we'd have to express this restriction in the type signature, which would be just noise).

It's not a big deal to encode "whenever `a` is non-functional, so is `b`", however we also want

- whenever it's not obvious whether `a` is non-functional, just ignore it (the hard part is that "not obvious whether" is encoded as a stuck type family application)
- whenever `b` is known to be either functional or not (i.e. `IsFun b` is not a stuck type family application), ignore `a`

In order to express all of that we only need to change the `ApN` instance as follows:

```haskell
type family And (a :: Bool) (b :: Bool) :: Bool
type instance And 'False _      = 'False
type instance And 'True  b      = b
type instance And _      'False = 'False
type instance And a      'True  = a

instance
        ( Applicative f
        , ifa ~ IsFun a, ifb ~ IsFun b, ifb ~ And ifa ifb
        , DispatchApN f a b ifb
        ) => ApN f a b where
    apN = dispatchApN (proxy# :: Proxy# ifb)
```

We still dispatch on `IsFun b`, but there's now a new constraint: `ifb ~ And ifa ifb`. We have three cases:

1. `ifa` is functional => `ifa` equals `'True` => the constraint reduces to `ifb ~ ifb`, which is trivially true
2. `ifa` is non-functional => `ifa` equals `'False` => the constraint reduces to `ifb ~ 'False`, which is the constraint we wanted to get
3. `ifa` is stuck. In that case we look at `ifb` in the definition of `And` just to reduce that constraint away, so that it doesn't show up in type signatures. When `b` is functional, we also require `a` to be functional, because the constraint reduces to `ifb ~ ifa` and `ifb` is known to be `'True`, but this is not an important requirement (as we anyway impose it down the pipeline in the functional instance of the dispatching class), so we really provide the last two clauses of `And` just to remove the useless constraint. If `ifb` is also stuck, then we'll get an incomprehensible error message, because there is no info we could use to drive instance resolution.

With this setup not only does the polymorphic case type check:

```haskell
poly :: (Enum a, Num a) => [a]
poly = liftAn (+) [1..5] [3..5]
```

but also the monomorphic one:

```haskell
mono = liftAn (++) (Just "a") (Just "b")
```

and no type signature is needed for the latter. See the [full code](src/Bonus.hs).

I wasn't aware before that GHC allows overlapping clauses in type family definitions, this is quite a nice feature to have.

To my surprise, if I replace the definition of `And` with

```haskell
type family And (a :: Bool) (b :: Bool) :: Bool where
    And 'True 'True = 'True
    And _     _     = 'False
```

the code also type checks. I don't understand how GHC is able to resolve everything in this case, so I guess I should go read some papers on the design of type families.

## Solutions with type families

[One solution](http://hackage.haskell.org/package/arity-generic-liftA-0.1.0.0/docs/src/Control.Applicative.Lift.Internal.html) to the arity-generic `liftA` problem is to count the number of `->`s in the type of the function being lifted using a type family and stop once a non-`->` is reached. Then we can drive instance resolution by the computed number of arguments. This solution breaks out of the overlapping instances problem by just not allowing the final `f` to be `(->) x`, see how the base case is handled:

```haskell
instance (CountArgs a ~ Z) => Applyable a Z where
```

This essentially breaks the polymorphic case like

```haskell
test :: (Enum a, Num a) => [a]
test = liftAn (+) [1..5] [3..5]
```

as `a` must be non-functional now and we have to add the `CountArgs a ~ Z` constraint. It is possible not to impose this restriction (see the [TF](src/TF.hs) module), but then the user will have to explicitly specify the number of arguments (via a type-level number), which is pretty annoying and defeats the purpose of the generic encoding.

However we can adapt the first solution presented in this post and use type families to count the number of `->`s in `f a -> f b -> ...` and then dispatch on the resulting natural, in which case we no longer need the mutual type classes trick anymore. See the [BackwardsTF](src/BackwardsTF.hs) module for details. But it's not clear whether it's possible to adapt this solution to infer types in the monomorphic case like we did in the previous section.

If you liked the post and appreciate the effort, consider [sponsoring](https://github.com/sponsors/effectfully-ou) this blog (starts from 1$).
