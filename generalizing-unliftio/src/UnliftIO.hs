{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module UnliftIO where

import           SomeAction

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Trans.Reader

class MonadIO m => MonadUnliftIO m where
    withRunInIO :: ((forall a. m a -> IO a) -> IO r) -> m r

instance MonadUnliftIO IO where
    withRunInIO k = k id

instance MonadUnliftIO m => MonadUnliftIO (ReaderT e m) where
    withRunInIO k = ReaderT $ \r -> withRunInIO $ \runInIO -> k $ runInIO . flip runReaderT r

liftU :: MonadUnliftIO m => IO r -> m r
liftU a = withRunInIO $ \_ -> a

forkU :: MonadUnliftIO m => m () -> m ThreadId
forkU a = withRunInIO $ \runInIO -> forkIO $ runInIO a

newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)
      deriving anyclass (SomeAction)

testApp :: ExceptT () App ()
testApp = throwError () `catchError` \() -> do
    _ <- lift $ forkU someAction
    printM ()

newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadError e, MFunctor)
      deriving anyclass (SomeAction)

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwError () `catchError` \() -> do
    _ <- hoist lift $ forkU someAction
    printM ()
