# Generalizing `unliftio`

In this post we'll go on a journey of generalizing the [`MonadUnliftIO`](https://hackage.haskell.org/package/unliftio-core-0.2.0.1/docs/Control-Monad-IO-Unlift.html#t:MonadUnliftIO) class, which is the main concept behind the `unliftio` [package](https://hackage.haskell.org/package/unliftio), with the intention to make the core type class more expressible without losing any safety of the approach.

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

One (intentional) limitation of `unliftio` is that it's not possible (other than by using [uncomfortable tricks](https://github.com/fpco/unliftio/issues/68)) to provide a `MonadUnliftIO` instance of `ContT`, `StateT`, `ExceptT` and everything else that is not isomorphic to `ReaderT R` for some `R`, so dealing with `ExceptT` requires extra plumbing. For one example, wrapping an `App` monad

```haskell
newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)
```

with `ExceptT` and calling `forkU` within the resulting monad requires an explicit `lift`:

```haskell
testApp :: ExceptT () App ()
testApp = throwError () `catchError` \() -> do
    _ <- lift . forkU $ printM ()
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
```

requires even more awkward plumbing:

```haskell
testAppT :: AppT (ExceptT () IO) ()
testAppT = throwError () `catchError` \() -> do
    _ <- hoist lift . forkU $ printM ()
    printM ()
```

where `hoist` comes from the [`mmorph`](https://hackage.haskell.org/package/mmorph) package and allows us to map the monad that the `AppT` transformer receives.

It would be great if neither `lift` in the former example, nor `hoist lift` in the latter were required and it would be even better if we could have some kind of general

```haskell
testAppG :: <a_bunch_of_constraints> => m ()
testAppG = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU $ printM ()
    printM ()
```

such that it was possible to instantiate it as both `testApp` and `testAppT`.

Our goal is to arrive at an abstraction allowing for such generality.

## Drop the restriction on the base monad

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

`MonadUnliftable`, being a generalization of `MonadUnliftIO`, supports the basic `IO` and `ReaderT` instances:

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

`ExceptT` is our first non-dischargeable monad transformer.

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
    _ <- lift . forkU $ printM ()
    printM ()
```

and implement its `MonadUnlift`-flavoured twin (note the `U` in `throwErrorU` and `catchErrorU`):

```haskell
testAppU :: ExceptT () App ()
testAppU = throwErrorU () `catchErrorU` \() -> do
    _ <- lift . forkU $ printM ()
    printM ()
```

for the same `App`:

```haskell
newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable)
```

Although that does not give us any advantage over the previous version as we still have to `lift` manually.

What we do win however is that in the other test

```haskell
newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable, MFunctor)

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwErrorU () `catchErrorU` \() -> do
    _ <- hoist lift . forkU $ printM ()
    printM ()
```

we no longer need to derive `MonadError` for `AppT`. Having `MonadUnliftable` is enough to be able to use `throwErrorU` and `catchErrorU`, which first unlift the monad and only then deal with the actual errors stuff.

I.e. the general `MonadUnliftable` makes it possible to derive/implement a single instance for a monad transformer and get the `MonadUnliftable`-flavoured versions of all the `MonadError`, `MonadState`, `MonadWriter` etc stuff for free.

Still that nasty `hoist lift` though. Hence the next section.
