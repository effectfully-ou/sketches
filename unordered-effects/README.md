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

## Core ([full code](src/Main.hs))

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

which reads as "every `eff` from `effs` is in the sum type `f`". I.e. `f` is the sum of all `effs`, e.g. if our effects are `Reader Bool`, `State String` and `Either Char`, then their sum is

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

is the same constraint as

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

(in the actual code we also have an irrelevant `Proxy` that helps to define complex operations, I'm omitting it in the README)

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

That is, as long as for any `f` `Sig f effs` implies `Call f eff`, we can be confident that `eff` is in `effs`. We use the [`QuantifiedConstraints`](https://gitlab.haskell.org/ghc/ghc/wikis/quantified-constraints) language extension here. Note how we essentially encode an n-ary sum constraint as a function from an n-ary product constraint!

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

The thing here is that at the call site the type of `Right "abc"` is underspecified: it's `Either a0 String` for some unspecified `a0`. We know the `a0` is supposed to be `Char`, because we have only one `Either` effect in the row of effects, but we haven't communicated that reasoning to the compiler. So let's do that.

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
- `Unify` collects arguments in a deeply nested tuple and once the heads match, calls `UnifyArgs`
- `UnifyArgs` turns collected arguments into constraints

The definition of `send` with better type inference is this then:

```haskell
sendFind :: (Find eff effs, eff `Member` effs) => eff a -> EffT effs m a
sendFind = send
```

I.e. we only add the `Find eff effs` constraint. This makes the example from the above type check.

Note however that if you have two effects with the same head (which is allowed), `sendFind` won't help, you'll have to specify manually which one of the effects you want.

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

Here is our aforementioned contrived example in its full:

```haskell
a1 :: Monad m => EffT '[Either Char, Reader Bool] m Int
a1 = local not $ do
    b <- sendFind ask
    if b
        then return 0
        else send $ Left 'a'

a2 :: EffT '[Reader Bool, Either Char] m Int
a2 = sendFind $ Right 1

a3 :: Monad m => EffT '[State String, Reader Bool] m Int
a3 = do
    sendFind $ modify (++ "text")
    length <$> sendFind get

a123 :: Monad m => EffT '[Either Char, Reader Bool, State String] m Int
a123 = do
    x1 <- embed a1
    x2 <- embed a2
    x3 <- embed a3
    return $ x1 + x2 + x3
```

In this section we'll interpret the example using a particular data type. This one:

```haskell
data TestF b
    = TestReader (Reader Bool   b)
    | TestState  (State  String b)
    | TestEither (Either Char   b)
```

It's the sum of all the effects of `a123`. We need to provide the `Call` instances:

```haskell
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

and once that is done, writing an interpreter is easy:

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

Here we inject each of the effects into the `ReaderT Bool (StateT String (Either Char))` monad (using the `transformers` and [`mmorph`](http://hackage.haskell.org/package/mmorph-1.1.3/docs/Control-Monad-Morph.html) packages).

A couple of tests

```haskell
-- Left 'a'
test1' :: Either Char Int
test1' = evalStateT (runReaderT (runExample a123) True) ""

-- Right 5
test2' :: Either Char Int
test2' = evalStateT (runReaderT (runExample a123) False) ""
```

evaluate correctly.

Of course constructing sums of effects and then injecting effects into them manually is very tedious. Hence the next section.

## `fastsum` + `mtl`

We need a class for interpreting an effect in a monad

```haskell
class InterpretIn m eff where
    interpret :: eff a -> m a
```

relevant instances

```haskell
instance Mtl.MonadReader r m => InterpretIn m (Reader r) where
    interpret a = runReader a <$> Mtl.ask

instance Mtl.MonadError e m => InterpretIn m (Either e) where
    interpret = Mtl.liftEither

instance Mtl.MonadState s m => InterpretIn m (State s) where
    interpret a = do
        s <- Mtl.get
        let (x, s') = runState a s
        Mtl.put s'
        return x
```

and an open sum type (we use the one from the [`fastsum`](https://hackage.haskell.org/package/fastsum-0.1.1.1) package, because it's fast and provides just the right API):

```haskell
instance eff :< effs => Call (Sum effs) eff where
    _Call = prism' inject project
```

(`Sum`, `(:<)`, `inject` and `project` come from the `fastsum` package)

Having these things in place, defining the main function is simple:

```haskell
runEffT
    :: forall effs m a. (Apply (InterpretIn m) effs, Sig (Sum effs) effs)
    => EffT effs m a -> m a
runEffT (EffT k) = k $ apply @(InterpretIn m) @effs interpret
```

(`Apply` and `apply` come from the `fastsum` package)

So we just instantiate the lifter with a function that interprets each effect from the sum of effects using the `interpret` function from the `InterpretIn` class.

The `Sig (Sum effs) effs` constraint must always be satisfied for any particular `effs`. It says that each effect from `effs` is in the n-ary sum of `effs`, which has to be true. We could even discharge such a constraint using some type-level [hasochism](http://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf), but I'd rather not.

## Alternative representation

We can define `Lifter` as

```haskell
type Lifter effs m = forall eff b. eff `Member` effs => eff b -> m b
```

and still make everything work, because `Member` remains the same "unordered" thing:

```haskell
class (forall f. Sig f effs => Call f eff) => eff `Member` effs
instance (forall f. Sig f effs => Call f eff) => eff `Member` effs
```

This definition of `Lifter` reads much better and it's a bit more convenient to work with, but I find it harder to reason about the quantified constraint and predict how constraints in various situations will resolve.

There might be a better way to express an n-ary sum in terms of an n-ary product than

```haskell
forall f. Sig f effs => Call f eff
```

For example,

```haskell
class (forall constr. All constr effs => constr eff) => eff `Member` effs
instance (forall constr. All constr effs => constr eff) => eff `Member` effs

class (forall eff. eff `Member` effs1 => eff `Member` effs2) => effs1 `In` effs2
instance (forall eff. eff `Member` effs1 => eff `Member` effs2) => effs1 `In` effs2
```

also seems to work and has some better behavior.

More thinking is required.

## Higher-order effects ([full code](src/HO.hs))

Is it possible to tweak the system to allow [higher-order effects](https://github.com/fused-effects/fused-effects#higher-order-effects)? Yep, we only need to go from functors to higher-order functors as [they do](http://hackage.haskell.org/package/fused-effects-1.0.0.0/docs/Control-Effect-Class.html#t:HFunctor) in the `fused-effects` library.

Here is an example:

```haskell
newtype HO eff m a = HO
    { unHO :: eff a
    }

data Local r m a = Local
    { _localFun :: r -> r
    , _localArg :: m a
    }

a1 :: Monad m => EffT '[HO (Either Char), HO (Reader Bool), Local Bool] m Int
a1 = send $ Local not $ do
    b <- send @(HO (Reader Bool)) $ HO ask
    if b
        then return 0
        else send $ HO $ Left 'a'
```

- `HO` turns a first-order effect into a higher-order one by ignoring the higher-order part (`m`)
- `Local` represents the `local` function that we considered above, only now it's an actual abstract effect that you need to interpret
- `a1` is an effectful computation that has the `Local` effect

Note that we `send` something that already has a couple of `send`s in it. Does that mean that `EffT` is now recursive? Yes. Does that mean that in order to embed a computation into a bigger row of effects we have to traverse it recursively? Nope! It's still

```haskell
embed :: effs1 `In` effs2 => EffT effs1 m a -> EffT effs2 m a
embed (EffT k) = EffT k
```

despite `EffT` being recursive. This is because instead of requiring the inner row of effects to be the same as the outer one (which would force us to traverse `EffT` recursively), we only require the inner one to be included in the outer one:

```haskell
type Lifter effs m
    =  forall effs' b
    .  effs' `In` effs
    => Proxy effs'
    -> (forall h. Sig h effs' => h (EffT effs' m) b)
    -> m b

newtype EffT effs m a = EffT
    { unEff :: Lifter effs m -> m a
    }
```

and

```
effs' `In` effs1  -- comes from `Lifter`
effs1 `In` effs2  -- comes from `embed`
```

together imply ``effs' `in` effs2``, so no recursion is needed.

Unfortunately, there is no higher-order version of `fastsum`, so in [the code](src/HO.hs) we only have an example with a hardcoded n-ary sum functor.

**UPDATE** [Denis Stoyanov](https://github.com/xgrommx) has pointed out that the [`haskus-utils-variant`](https://hackage.haskell.org/package/haskus-utils-variant) package provides a [higher-order open sum type](https://hackage.haskell.org/package/haskus-utils-variant-3.0/docs/Haskus-Utils-EGADT.html#t:HVariantF) (as well as a [first-order one](https://hackage.haskell.org/package/haskus-utils-variant-3.0/docs/Haskus-Utils-VariantF.html#t:VariantF)).

## Trouble in paradise

As you probably already realized, I'm not defining any common algebraic effects functions in this post. Like this one:

```haskell
interpose
    :: (Monad m, eff `Member` effs)
    => (forall a. eff a -> EffT effs m a) -> EffT effs m a -> EffT effs m a
```

or this one:

```haskell
interpret
    :: Monad m
    => (forall a. eff a -> EffT effs m a) -> EffT (eff ': effs) m a -> EffT effs m a
```

This is because it is incredibly hard to do so. It took me a couple of hours to define the former and after several hours of struggle I'm still not able to define the latter, because dealing with ordered things when your internals are unordered is even harder than dealing with unordered things when your internals are ordered.

So what is presented here is not a proper effect system. It might become one some day, but currently it's just a layer that allows to get unorderness.

## Conclusions

The post is more of an exploratory kind. We've explored a bit the unorderness concept, but there's much more to it.

See the full code of the main first-order version [here](src/Main.hs).

If you liked this post and appreciate the effort, consider becoming a [sponsor](https://github.com/sponsors/effectfully-ou) (starts from 1$).
