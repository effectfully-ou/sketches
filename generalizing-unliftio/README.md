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

For another example putting `ExceptT` inside an `AppT` transformer

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
    _ <- forkU $
        printM ()
    printM ()
```

such that it was possible to instantiate it as both `testApp` and `testAppT`.

Our goal is to arrive at an abstraction allowing for such generality.

## Drop the restriction on the base monad

always being `IO`
