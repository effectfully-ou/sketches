# `unordered-effects`

This post shows a technique that allows to encode unordered things at the type-level. We'll use algebraic effects as an example as it's a common use case that requires some notion of unorderness.

Algebraic effects libraries commonly provide a `Member` type class that allows to write things like

```haskell
a :: (Error Char `Member` effs, Env Bool `Member` effs) => Eff effs Int
```

However once you instantiate `effs` to a particular list of effects, going back from

```haskell
a' :: Eff '[Error Char, Env Bool] Int
```

to the original `a` requires traversing `Eff` (which is usually some kind of free, freer, freerish, etc monad).

Here we'll describe an encoding with _intrinsically unordered_ effects. I.e. reordering a particular list of effects is a no-op, as well as embedding it into a superlist.

We'll be using the following example throughout the post:

```haskell
a1 :: Monad m => EffT '[Either Char , Reader Bool] m Int
a2 :: Monad m => EffT '[Reader Bool , Either Char] m Int
a3 :: Monad m => EffT '[State String, Reader Bool] m Int

a123 :: Monad m => EffT '[Either Char, Reader Bool, State String] m Int
a123 = do
    x1 <- embed a1
    x2 <- embed a2
    x3 <- embed a3
    return $ x1 + x2 + x3
```

Here `a1`, `a2` and `a3` are some computations that all run with distinct / distinctly ordered effects and `a123` embeds all of `a*` into a single row of effects using the `embed` function, which, as promised, is a no-op:

```haskell
embed :: effs1 `In` effs2 => EffT effs1 m a -> EffT effs2 m a
embed (EffT k) = EffT k
```

(`EffT` is a `newtype` wrapper).

## Core

Since we're encoding regular algebraic effects, they're going to be functors (we'll also consider higher-order effects and functors later):

```haskell
type Effect = * -> *
```

As is common, effects will be packed in an n-ary sum type, but we take an abstract approach here and instead of hardcoding any particular open sum type, use prisms over an abstract `f`:

```haskell
class Call f (eff :: Effect) where
    _Call :: Prism' (f a) (eff a)
```

This translates to

- `_Call # eff` lifts an `eff a` into an `f a`
- `a ^? _Call` unlifts an `f a` down to `Maybe (eff a)`

Having this in place, we can define a "signature":

```haskell
type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    All _ '[]       = ()
    All f (x ': xs) = (f x, All f xs)

class All (Call f) effs => Sig f effs
instance All (Call f) effs => Sig f effs
```

which reads as "every `eff` from `effs` is in the sum type `f`". I.e. `f` is the sum of all `effs`, e.g. if our effect are `Reader Bool`, `State String` and `Either Char`, then their sum is

```haskell
data TestF b
    = TestReader (Reader Bool   b)
    | TestState  (State  String b)
    | TestEither (Either Char   b)
```

Note however that the `All (Call f) effs` constraint only says that every `eff` is in `f`, but it doesn't say that `f` consists only of the effects from `effs`: `f` might have some additional irrelevant effects.

As an example

```haskell
Sig f '[Reader Bool, State String, Either Char]
```

is the constraint as

```haskell
(Call f (Reader Bool), Call f (State String), Call f (Either Char))
```

which is the same constraint as

```haskell
(Call f (Either Char), Call f (Reader Bool), Call f (State String))
```

or any other permutation of the subconstraints. This is how we achieve unorderness.

Here is how we define the type of a computation that performs a single effect from a particular list of effects:

```haskell
comp :: forall f. Sig f effs => f b
```

Next we define the type of functions that allow to interpret an effectful computation in a monad:

```haskell
type Lifter effs m = forall b. (forall f. Sig f effs => f b) -> m b
```

This is the most important part of the encoding. A `Lifter effs m` receives a computation that returns a `b` and can perform _a single effect_ from `effs` (and only from `effs`, which we ensure by being parametric over `f`). `Lifter` generalizes all of these functions:

```haskell
liftEither :: Either e a -> M a
liftReader :: Reader r a -> M a
liftState  :: State  s a -> M a
```

(where `M` is some particular monad that we can interpret all the effects in) and many more. I.e. you can turn a

```haskell
Lifter '[Either e, Reader r, State s] m
```

into any of the functions from the above. For example having

```haskell
lifter :: Lifter '[Either Char, Reader Bool, State String] M
```

we can define this function:

```haskell
liftR :: Reader Bool a -> M a
liftR eff = lifter $ _Call # eff
```

and a similar one for each of the other effects.

Note also that the `M` doesn't have to be a particular monad, we can generalize all of those:

```haskell
liftEither :: MonadError  e m => Either e a -> m a
liftReader :: MonadReader r m => Reader r a -> m a
liftState  :: MonadState  s m => State  s a -> m a
```

i.e. interpret the effects using the classes provided by `mtl`. `Lifter` (and the entire encoding) doesn't impose any particular effect system as a target to compile to, but we'll be using `mtl` for examples as it's a lingua franca of effect systems and it's easy to target it.

Our algebraic effects transformer is this:

```haskell
newtype EffT effs m a = EffT
    { unEff :: Lifter effs m -> m a
    }
```

I.e. it's the same thing as `ReaderT (Lifter effs m) m` (modulo the fact that that would require `ImpredicativePolymorphism`).

The definition of `EffT` reads as "compute a value in the `m` monad, given a function that interprets all the effects from `effs` in that monad". Here is a contrived example of how we can construct such an effectful computation manually:

```haskell
comp :: Monad m => EffT '[Either Char, Reader Bool] m Int
comp = EffT $ \lifter -> do
    b <- lifter $ _Call # (ask :: Reader Bool Bool)
    if b
        then return 0
        else lifter $ _Call # Left 'a'
```

Here we lift the `Reader Bool` and `Either Char` effects into the mothership monad `m`, which is completely abstract. Any `m` is suitable as long as you can provide a function that interprets both the effects in it. We had to specify the type of `ask`, but worry not, type inference problems will be addressed as well.

Of course binding `lifter` and using prismatic thingies manually is tedious, so let's abstract those implementation details out.

## Membership and inclusion

The membership relation is defined as

```haskell
class (forall f. Sig f effs => Call f eff) => eff `Member` effs
instance (forall f. Sig f effs => Call f eff) => eff `Member` effs
```

That is, as long as for any `f` `Sig f effs` implies `Call f eff`, we can be confident that `eff` is in `effs`. We use the [`QuantifiedConstraints`](https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints) language extension here.

Using this type class we can abstract the details of the encoding out:

```haskell
send :: eff `Member` effs => eff a -> EffT effs m a
send a = EffT $ \lifter -> lifter $ _Call # a
```

Now the example from the above becomes:

```haskell
comp :: Monad m => EffT '[Either Char, Reader Bool] m Int
comp = do
    b <- send (ask :: Reader Bool Bool)
    if b
        then return 0
        else send $ Left 'a'
```

which is a bit nicer.

Similarly, the inclusion relation is

```haskell
class (forall f. Sig f effs2 => Sig f effs1) => effs1 `In` effs2
instance (forall f. Sig f effs2 => Sig f effs1) => effs1 `In` effs2
```

And as mentioned above, `embed` is

```haskell
embed :: effs1 `In` effs2 => EffT effs1 m a -> EffT effs2 m a
embed (EffT k) = EffT k
```

There is a downside to this encoding, though: we'd like ``'[eff] `In` effs`` to imply ``eff `Member` effs``, but this is not the case as

```haskell
inMember :: '[eff] `In` effs => Proxy ('(,) eff effs) -> (eff `Member` effs => c) -> c
inMember _ = id
```

gives

```
    • Could not deduce (Call f eff)
      from the context: In '[eff] effs
```

At least it works the other way around:

```haskell
memberIn :: eff `Member` effs => Proxy ('(,) eff effs) -> ('[eff] `In` effs => c) -> c
memberIn _ = id
```

And we can cover the first use case by introducing a separate type class:

```haskell
class All (Flip Member effs2) effs1 => effs1 `Members` effs2
instance All (Flip Member effs2) effs1 => effs1 `Members` effs2
```

Then entailments work both the ways:

```haskell
membersMember2
    :: '[eff1, eff2] `Members` effs
    => Proxy ('(,,) eff1 eff2 effs) -> ((eff1 `Member` effs, eff2 `Member` effs) => c) -> c
membersMember2 _ = id

memberMembers2
    :: (eff1 `Member` effs, eff2 `Member` effs)
    => Proxy ('(,,) eff1 eff2 effs) -> ('[eff1, eff2] `Members` effs => c) -> c
memberMembers2 _ = id
```

and we can go from `Members` to `In` for a list of effects with statically known length:

```haskell
membersIn2
    :: '[eff1, eff2] `Members` effs
    => Proxy ('(,,) eff1 eff2 effs) -> ('[eff1, eff2] `In` effs => c) -> c
membersIn2 _ = id
```

So as always there are trade-offs involved, but there also might exist a more convenient encoding, I haven't really investigated this.

## Improving type inference

Consider the following snippet:

```haskell
comp :: EffT '[Reader Bool, Either Char] m String
comp = send $ Right "abc"
```

It doesn't type check:

```haskell
    • Could not deduce (Call f (Either a0))
        arising from a use of ‘send’
```

The thing here is that at the call site the type of `Right 1` is underspecified: it's `Either a0 String` for some unspecified `a0`. We know the `a0` is supposed to be `Char`, because we have only one `Either` effect in the row of effects, but we haven't communicated that reasoning to the compiler. So let's do that.

The idea is the following: we try to unify an `eff` effect with each of the effects from an `effs` row of effects by checking for equality the head of `eff` (the head of `f x y z` is `f`) and the head of each of the effects from `eff`. Once a matching head is found, we add constraints unifying the arguments (that the heads are applied to) element-wise.

In the example above this becomes

```
Find (Either a0) '[Reader Bool, Either Char]                       ~>  -- [1]
(Unify (Either a0) (Reader Bool), Unify (Either a0) (Either Char)) ~>  -- [2]
Unify (Either a0) (Either Char)                                    ~>  -- [3]
a0 ~ Char
```

- [1] simply unfolds the definition of `Find`
- [2] discards the first constraint, because the head of `Either a0` (`Either`) doesn't unify with the head of `Reader Bool` (`Reader`)
- [3] checks that the heads match and adds a constraint unifying `a0` with `Char`

The full code looks like that:

```haskell
type family UnifyArgs (args :: k) :: Constraint where
    UnifyArgs '()                    = ()
    UnifyArgs ('(,) ('(,) x y) args) = (x ~ y, UnifyArgs args)

type family Unify (args :: m) (x :: k) (y :: l) :: Constraint where
    Unify args (f x) (g y) = Unify ('(,) ('(,) x y) args) f g
    Unify args x     x     = UnifyArgs args
    Unify args _     _     = ()

type family Find (x :: k) (xs :: [k]) :: Constraint where
    Find x '[]       = ()
    Find x (y ': xs) = (Unify '() x y, Find x xs)
```

- `Find` calls `Unify` for each element of the list
- `Unify` collects arguments in a deeply nested tuple and once heads match, calls `UnifyArgs`
- `UnifyArgs` turns collected arguments into constraints

The definition of `send` with better type inference is this then:

```haskell
sendFind :: (Find eff effs, eff `Member` effs) => eff a -> EffT effs m a
sendFind = send
```

I.e. we only add the `Find eff effs` constraint. This makes the example from the above type check.

## A higher-order effectful function: `local`

The good thing about this encoding is that you can always fall back to `transformers` or `mtl`. For example we can define a `MonadReader` instance for `EffT` as follows:

```haskell
instance Mtl.MonadReader r m => Mtl.MonadReader r (EffT effs m) where
    ask = lift Mtl.ask
    local f (EffT k) = EffT $ \b -> Mtl.local f $ k b
```

The instance just delegates to the `MonadReader` instance of the underlying `m` monad.

And you can stack `ContT` or whatever fancy transformer on top of `m` or constrain `m` with `MonadCont` and that will still allow you to use unordered effects and interpret them in `m`. So the encoding does not restrict you to use only first-order effects: you can also use higher-order effects perfectly well -- just not in the first-order part of the encoding.

As with other effect systems (see e.g. [`Control.Monad.Freer.Reader.local`](https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Reader.html#v:local) or [`Control.Eff.Reader.Lazy.local`](https://hackage.haskell.org/package/extensible-effects-5.0.0.1/docs/Control-Eff-Reader-Lazy.html)) we can define higher-order effectful functions (as opposed to higher-order effects whose interpretation is not hardcoded). For example having

```haskell
mapEff :: eff `Member` effs => (forall a. eff a -> eff a) -> EffT effs m a -> EffT effs m a
```

which applies a function to a certain effect from the row of effects, we can define

```haskell
local :: Reader r `Member` effs => (r -> r) -> EffT effs m a -> EffT effs m a
local f = mapEff @(Reader _) $ Mtl.local f
```

## Example

```haskell
a1 :: Monad m => EffT '[Either Char, Reader Bool] m Int
a1 = do
    b <- send ask
    if b
        then return 0
        else send $ Left 'a'

a2 :: EffT '[Reader Bool, Either Char] m Int
a2 = send $ Right 1

a3 :: Monad m => EffT '[State String, Reader Bool] m Int
a3 = do
    send $ modify (++ "text")
    length <$> send get

a123 :: Monad m => EffT '[Either Char, Reader Bool, State String] m Int
a123 = do
    x1 <- embed a1
    x2 <- embed a2
    x3 <- embed a3
    return $ x1 + x2 + x3
```



```haskell
data TestF b
    = TestReader (Reader Bool   b)
    | TestState  (State  String b)
    | TestEither (Either Char   b)

instance Call TestF (Reader Bool) where
    _Call = prism' TestReader $ \case
        TestReader b -> Just b
        _            -> Nothing

instance Call TestF (State String) where
    _Call = prism' TestState $ \case
        TestState b -> Just b
        _           -> Nothing

instance Call TestF (Either Char) where
    _Call = prism' TestEither $ \case
        TestEither b -> Just b
        _            -> Nothing
```

```haskell
runExample
    :: m ~ ReaderT Bool (StateT String (Either Char))
    => EffT '[Either Char, Reader Bool, State String] m Int
    -> m Int
runExample (EffT k) = k $ \case
    TestReader b -> hoist generalize b
    TestState  b -> lift $ hoist generalize b
    TestEither b -> lift $ lift b
```
