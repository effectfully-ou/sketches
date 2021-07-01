# Generalizing `unliftio`

In this post we'll go on a journey of generalizing the [`MonadUnliftIO`](https://hackage.haskell.org/package/unliftio-core-0.2.0.1/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO) class, which is the main concept behind the `unliftio` [package](https://hackage.haskell.org/package/unliftio), with the intent to make the core type class more expressible without losing any safety of the approach.

This post is not a tutorial on `unliftio`, so the reader is assumed to be familiar with the library (if you're not, check out the [readme](https://github.com/fpco/unliftio#readme) of the package).

Disclaimer: this post is written for exploration purposes, no suggestion of practical usage is intended.

## The baseline ([full code](./src/UnliftIO.hs))

To recap, `MonadUnliftIO` is defined as follows:

```haskell
class MonadIO m => MonadUnliftIO m where
    withRunInIO :: ((forall a. m a -> IO a) -> IO r) -> m r
```

and the two core instances are

```haskell
instance MonadUnliftIO IO where
    withRunInIO k = k id

instance MonadUnliftIO m => MonadUnliftIO (ReaderT e m) where
    withRunInIO k = ReaderT $ \r -> withRunInIO $ \runInIO -> k $ runInIO . flip runReaderT r
```

An example definition is `MonadUnliftIO`-powered version of `forkIO`:

```haskell
forkU :: MonadUnliftIO m => m () -> m ThreadId
forkU a = withRunInIO $ \runInIO -> forkIO $ runInIO a
```

Regular `forkIO` takes an `IO` action and returns an `IO` action. `liftIO . forkIO` takes an `IO` action and returns an action running in some `MonadIO`. `forkU` takes an action running in some `MonadUnliftIO` and returns an action running in the same monad. For example, if `m` supports some kind of action

```haskell
class MonadIO m => SomeAction m where
    someAction :: m ()
    someAction = liftIO $ putStrLn "performed some action"
```

then `forkU`'s argument is allowed to invoke that action:

```haskell
forkSomeAction :: (MonadUnliftIO m, SomeAction m) => m ThreadId
forkSomeAction = forkU someAction
```

which would be impossible with `forkIO` or `liftIO . forkIO` due to them hardcoding the inner action to run in bare-bones `IO`.

One (intentional) limitation of `unliftio` is that it's not possible (other than by using [uncomfortable tricks](https://github.com/fpco/unliftio/issues/68)) to provide a `MonadUnliftIO` instance of `ContT`, `StateT`, `ExceptT` and everything else that is not isomorphic to `ReaderT R` for some `R`, so dealing with `ExceptT` requires extra plumbing. For one example, wrapping an `App` monad

```haskell
newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)
      deriving anyclass (SomeAction)
```

with `ExceptT` and calling `forkU` within the resulting monad requires an explicit `lift`:

```haskell
testApp :: ExceptT () App ()
testApp = throwError () `catchError` \() -> do
    _ <- lift $ forkU someAction
    printM ()
```

where `printM` is a version of `print` that works for any `MonadIO`:

```haskell
printM :: (MonadIO m, Show a) => a -> m ()
printM = liftIO . print
```

For another example, putting `ExceptT` inside an `AppT` transformer

```haskell
newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadError e, MFunctor)
      deriving anyclass (SomeAction)
```

requires even more awkward plumbing:

```haskell
testAppT :: AppT (ExceptT () IO) ()
testAppT = throwError () `catchError` \() -> do
    _ <- hoist lift $ forkU someAction
    printM ()
```

where `hoist` comes from the [`mmorph`](https://hackage.haskell.org/package/mmorph) package and allows us to map the monad that the `AppT` transformer receives.

It would be great if neither `lift` in the former example, nor `hoist lift` in the latter were required and it would be even better if we could have some kind of general

```haskell
testAppG :: <a_bunch_of_constraints> => m ()
testAppG = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU someAction
    printM ()
```

such that it was possible to instantiate it as both `testApp` and `testAppT`.

Our goal is to arrive at an abstraction allowing for such generality.

## Drop the restriction on the base monad ([full code](./src/Unlift.hs))

In `MonadUnliftIO` the base monad is always `IO`. One pretty obvious generalization is to drop this restriction and make the base monad an argument of the class:

```haskell
class (Monad b, Monad m) => MonadUnlift b m | m -> b where
    withUnlift :: ((forall a. m a -> b a) -> b r) -> m r
```

This is [what they do](https://github.com/kowainik/unlift/blob/132e8faa00a44f06dfeb2375fff6d77f64dc96b4/src/Unlift.hs#L66-L74) in the `unlift` library modulo the exact choice of names and superclasses.

I'm not a fan of `FunctionalDependencies`, so we'll be using the following definition:

```haskell
class (Monad (Unlift m), Monad m) => MonadUnliftable m where
    type Unlift m :: * -> *
    withUnlift :: ((forall a. m a -> Unlift m a) -> Unlift m r) -> m r
```

which allows us to recover the multi-parameter version via

```haskell
type MonadUnlift b m = (MonadUnliftable m, Unlift m ~ b)
```
**UPDATE**: note that it could also be

```haskell
class (Monad b, Monad m, b ~ Unlift m) => MonadUnlift b m where
    type Unlift m :: * -> *
    withUnlift :: ((forall a. m a -> b a) -> b r) -> m r
```

instead of `MonadUnliftable` as [suggested](https://github.com/effectfully-ou/sketches/issues/2) by [**treeowl**](https://github.com/treeowl), which is probably even better, but the post was originally written with `MonadUnliftable` and so the rest still talks about the original version of the class.

`MonadUnliftable`, being a generalization of `MonadUnliftIO`, supports the core `IO` and `ReaderT` instances:

```haskell
instance MonadUnliftable IO where
    type Unlift IO = IO  -- [1]
    withUnlift k = k id

instance MonadUnliftable m => MonadUnliftable (ReaderT r m) where
    type Unlift (ReaderT r m) = Unlift m  -- [2]
    withUnlift k = ReaderT $ \r -> withUnlift $ \unlift -> k $ unlift . flip runReaderT r
```

If we look closely, we'll see that unlifting `IO` does not remove `IO` from the stack ([1]), while unlifting `ReaderT` removes it from the stack ([2]). We will refer to monads that get removed by unlifting as "dischargeable" and to those that don't -- as "non-dischargeable".

Note that "runnability" and "dischargeability" are two different properties: `STM` is neither runnable (as in, there's no safe function of type `<...> -> STM a -> a`) nor dischargeable just like `IO`, but `ST s` and `Identity` are runnable and yet are non-dischargeable:

```haskell
instance MonadUnliftable (ST s) where
    type Unlift (ST s) = ST s
    withUnlift k = k id

instance MonadUnliftable Identity where
    type Unlift Identity = Identity
    withUnlift k = k id
```

(in the real world those instances should be [via-derived](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html)).

For another example, `IdentityT` is both runnable and dischargeable just like `ReaderT`:

```haskell
instance MonadUnliftable m => MonadUnliftable (IdentityT m) where
    type Unlift (IdentityT m) = m
    withUnlift k = IdentityT $ k runIdentityT
```

Apart from these straightforward instances that the generalization allows us to implement, we can also support something that was unthinkable before: a sane instance for `ExceptT` (and `StateT`, `WriterT` etc, but we're gonna focus on `ExceptT`):

```haskell
instance MonadUnliftable m => MonadUnliftable (ExceptT e m) where
    type Unlift (ExceptT e m) = ExceptT e (Unlift m)
    withUnlift k = ExceptT $ withUnlift $ \unlift -> runExceptT $ k $ mapExceptT unlift
```

The instance states that `ExceptT` can't be removed from the monadic stack for the purposes of unlifting and therefore the base monad has to contain it. Note that it's still possible to discharge, say, `ReaderT` underneath `ExceptT` due to the recursion in the instance, even though `ExceptT` is not dischargeable itself.

We can use the generalized class to provide unlifting versions of `throwError` and `catchError`:

```haskell
throwErrorU :: (MonadUnlift b m, MonadError e b) => e -> m a
throwErrorU = liftU . throwError

catchErrorU :: (MonadUnlift b m, MonadError e b) => m a -> (e -> m a) -> m a
a `catchErrorU` f = withUnlift $ \unlift -> unlift a `catchError` (unlift . f)
```

where `liftU` is a `MonadUnlift`-based equivalent of [`liftBase`](https://hackage.haskell.org/package/transformers-base-0.4.5.2/docs/Control-Monad-Base.html#v:liftBase)

```haskell
liftU :: MonadUnlift b m => b a -> m a
liftU a = withUnlift $ \_ -> a
```

Having all that machinery we can take the old test (which type checks with the generalized class as well)

```haskell
testApp :: ExceptT () App ()
testApp = throwError () `catchError` \() -> do
    _ <- lift $ forkU someAction
    printM ()
```

and implement its `MonadUnlift`-flavoured twin (note the `U` in `throwErrorU` and `catchErrorU`):

```haskell
testAppU :: ExceptT () App ()
testAppU = throwErrorU () `catchErrorU` \() -> do
    _ <- lift $ forkU someAction
    printM ()
```

for the same `App`:

```haskell
newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable)
      deriving anyclass (SomeAction)
```

Although that does not give us any advantage over the previous version as we still have to `lift` manually.

What we do win however is that in the other test

```haskell
newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable, MFunctor)
      deriving anyclass (SomeAction)

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwErrorU () `catchErrorU` \() -> do
    _ <- hoist lift $ forkU someAction
    printM ()
```

we no longer need to derive `MonadError` for `AppT`. Having `MonadUnliftable` is enough to be able to use `throwErrorU` and `catchErrorU`, which first `Unlift` the monad and only then deal with the actual errors stuff.

I.e. the general `MonadUnliftable` makes it possible to derive/implement a single instance for a monad transformer and get the `MonadUnliftable`-flavoured versions of all the `MonadError`, `MonadState`, `MonadWriter` etc stuff for free.

Still that nasty `hoist lift` though. Hence the next section.

## Allow the inner and the outer monads to differ ([full code](./src/UnliftPeel.hs))

In the previous version (the multi-parameteric type class flavour) the inner monad would always be the same as the outer one (`m` in both the cases):

```haskell
class (Monad b, Monad m) => MonadUnlift b m | m -> b where
    withUnlift :: ((forall a. m a -> b a) -> b r) -> m r
```

We can generalize that to (I'm using a multi-parameterc type class version for simplicity, we'll be back to type families shortly)

```haskell
class (Monad b, Monad m) => MonadUnliftPeel p b m | m b -> p, m p -> b where
    withUnliftPeel :: ((forall a. p a -> b a) -> b r) -> m r
```

Here the inner monad (`p`) is allowed to differ from the outer monad (`m`). The idea is that `p` is either equal to `m` like before or is some "peeled" version of `m` where we drop some parts of the outer stack for the inner computation, so that the type class machinery can do all the lifting for us.

The two core instances are like before:

```haskell
instance MonadUnliftPeel IO IO IO where
    withUnliftPeel k = k id

instance MonadUnliftPeel p b m => MonadUnliftPeel (ReaderT e p) b (ReaderT e m) where
    withUnliftPeel k = ReaderT $ \r -> withUnliftPeel $ \unlift -> k $ unlift . flip runReaderT r
```

but it gets fancier for `ExceptT` where we can now have two legitimate instances, one is like what we had before:

```haskell
instance {-# OVERLAPPING #-}
            MonadUnliftPeel p b m => MonadUnliftPeel (ExceptT e p) (ExceptT e b) (ExceptT e m) where
    withUnliftPeel k =
        trace "Keeping ExceptT" $  -- For showcasing the behavior, shouldn't appear in the real code.
            ExceptT $ withUnliftPeel $ \unlift -> runExceptT $ k $ mapExceptT unlift
```

and the other one simply drops `ExceptT` for the inner computation at the type level and calls `lift` at the term level:

```haskell
instance {-# OVERLAPPABLE #-} MonadUnliftPeel p b m => MonadUnliftPeel p b (ExceptT e m) where
    withUnliftPeel k =
        trace "Dropping ExceptT" $  -- For showcasing the behavior, shouldn't appear in the real code.
            lift $ withUnliftPeel k
```

Now having the usual infrastructure:

```haskell
printM :: (MonadIO m, Show a) => a -> m ()
printM = liftIO . print

forkU :: MonadUnliftPeel p IO m => p () -> m ThreadId
forkU a = withUnliftPeel $ \unlift -> forkIO $ unlift a

liftU :: MonadUnliftPeel m b m => b r -> m r
liftU a = withUnliftPeel $ \_ -> a

throwErrorU :: (MonadUnliftPeel m b m, MonadError e b) => e -> m a
throwErrorU = liftU . throwError

catchErrorU :: (MonadUnliftPeel m b m, MonadError e b) => m a -> (e -> m a) -> m a
catchErrorU a f = withUnliftPeel $ \unlift -> unlift a `catchError` (unlift . f)

newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO)
      deriving anyclass (SomeAction)

instance MonadUnliftPeel App IO App where
    withUnliftPeel k = App $ withUnliftPeel $ \unlift -> k $ unlift . unApp
```

we can replicate the old test:

```haskell
testApp1 :: ExceptT () App ()
testApp1 = throwErrorU () `catchErrorU` \() -> do
    _ <- lift $ forkU someAction
    printM ()
```

but also drop that `lift` and still have the example working:

```haskell
testApp2 :: ExceptT () App ()
testApp2 = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU someAction
    printM ()
```

Using a runner for `ExceptT () App`

```haskell
runExceptTApp :: ExceptT () App a -> IO ()
runExceptTApp = void . flip runReaderT () . unApp . runExceptT
```

we can check that in the original example with `lift` only the instance that keeps `ExceptT` is used (once for `throwErrorU` and once for `catchErrorU`):

```
>>> runExceptTApp testApp1
Keeping ExceptT
Keeping ExceptT
performed some action
()
```

while in the updated one with no explicit `lift` the instance that drops `ExceptT` (and calls `lift`) is used as well:

```
>>> runExceptTApp testApp2a
Keeping ExceptT
Keeping ExceptT
Dropping ExceptT
performed some action
()
```

And we can drop `hoist lift` from the other example too:

```haskell
newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving (Functor, Applicative, Monad, MonadIO)
      deriving anyclass (SomeAction)

instance MonadUnliftPeel p b m => MonadUnliftPeel (AppT p) b (AppT m) where
    withUnliftPeel k = AppT $ withUnliftPeel $ \unlift -> k $ unlift . unAppT

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU someAction
    printM ()
```

It's important to stress that `App` gets peeled to itself and `AppT` gets peeled to itself as well. This is because whatever our main monad is, we want to be able to feed actions running in that monad to `forkU`, so that we don't lose the benefits of `unliftio` discussed in the beginning of the post.

Note that the body of `testAppT` is literally the same as the one of `testApp2`. Given that, we can generalize both the examples to (the comments are constraints required by each individual line)

```haskell
testAppG
    :: ( MonadUnliftPeel m b m, MonadError () b  -- [1]
       , MonadUnliftPeel p IO m					 -- [2]
       , MonadIO p
       , MonadIO m
       ) => m ()
testAppG = throwErrorU () `catchErrorU` \() -> do  -- MonadUnliftPeel m b m, MonadError () b
    _ <-
        forkU                                      -- MonadUnliftPeel p IO m
            someAction                             -- SomeAction p
    printM ()                                      -- MonadIO m
```

(Note how `m` gets peeled to itself in the first `MonadUnliftPeel` constraint ([1]) and the same `m` gets peeled to `p` in the second one ([2]). This is how we handle this requirement for `m` to support both the keeping-`ExceptT` and the dropping-`ExceptT` instances)

And we can instantiate the general definition at both the concrete types:

```haskell
testApp2G :: ExceptT () App ()
testApp2G = testAppG

testAppTG :: AppT (ExceptT () IO) ()
testAppTG = testAppG
```

It works. Are we done though? Those overlapping instances are rather nasty (although we could probably avoid them by pattern matching at the type level on the peeled monad in a type family and choosing different routes depending on whether the peeled monad starts with `ExceptT` or not, see [Avoiding overlapping instances in the recursive case](https://github.com/effectfully/sketches/tree/master/avoid-overlapping-recursive) for a detailed description of the trick). It seems we're conflating two different concepts: "unlifting" and "peeling", so why not try separating them from each other?

## `MonadPeel` built on top of `MonadUnlift` ([full code](./src/Peel.hs))

Here's how it looks like if we keep the `MonadUnliftable` class from the "Drop the restriction on the base monad" section intact and instead of trying to generalize it any further, add a separate `MonadPeelable` class:

```haskell
class (Monad (Peel m), Monad m) => MonadPeelable m where
    type Peel m :: * -> *
    liftP :: Peel m a -> m a
```

with the usual recover-the-corresponding-multi-parameter-type-class-via-a-type-equality-constraint trick:

```haskell
type MonadPeel p m = (MonadPeelable m, Peel m ~ p)
```

`MonadPeelable` is not just [`MonadBase`](https://hackage.haskell.org/package/transformers-base-0.4.5.2/docs/Control-Monad-Base.html#t:MonadBase), it's intended to be the dual of `MonadUnliftable` (I've managed to successfully avoid calling it `MonadCounliftable` and `MonadLiftable` would be too general of a name).

`MonadUnliftable` discharges `ReaderT` and `AppT` and so `MonadPeelable` has to keep them:

```haskell
instance MonadPeelable m => MonadPeelable (ReaderT r m) where
    type Peel (ReaderT r m) = ReaderT r (Peel m)
    liftP = hoist liftP

instance MonadPeelable m => MonadPeelable (AppT m) where
    type Peel (AppT m) = AppT (Peel m)
    liftP = hoist liftP
```

in order for us to be able to use the environment of the outer monad in the inner action.

`MonadUnliftable` does not discharge `ExceptT` and so `MonadPeelable` has to drop it:

```haskell
instance MonadPeelable m => MonadPeelable (ExceptT e m) where
    type Peel (ExceptT e m) = Peel m
    liftP = lift . liftP
```

in order for us to get automatic lifting into the outer monad.

Non-recursive instances are trivial:

```haskell
instance MonadPeelable IO where
    type Peel IO = IO
    liftP = id

instance MonadPeelable Identity where
    type Peel Identity = Identity
    liftP = id

instance MonadPeelable App where
    type Peel App = App
    liftP = id
```

Given all of that we can define the general test

```haskell
testAppG
    :: ( MonadUnlift b m, MonadError () b
       , MonadPeel p m
       , MonadUnlift IO p
       , SomeAction p
       , MonadIO m
       )
    => m ()
testAppG = throwErrorU () `catchErrorU` \() -> do  -- MonadUnlift b m, MonadError () b
    _ <-
        liftP $                                    -- MonadPeel p m
            forkU $                                -- MonadUnlift IO p
                someAction                         -- SomeAction p
    printM ()                                      -- MonadIO m
```

and instantiate it at both the concrete types:

```haskell
testApp :: ExceptT () App ()
testApp = testAppG

testAppT :: AppT (ExceptT () IO) ()
testAppT = testAppG
```

without having an overly general three-parameter type class requiring overlapping instances for it to work.

## Conclusions

We've considered various ways of generalizing the `MonadUnliftIO` class. Nothing described here has been tested in any kind of real-world environment, but the ideas kinda seem promising.

If you liked the post and appreciate the effort, consider [sponsoring](https://github.com/sponsors/effectfully-ou) this blog (starts from 1$).
