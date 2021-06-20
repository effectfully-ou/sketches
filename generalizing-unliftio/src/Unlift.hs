{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Unlift where

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Trans.Reader

-- class (Monad n, Monad m) => MonadUnlift n m | m -> n where
--     withUnlift :: ((forall a. m a -> n a) -> n b) -> m r

class (Monad (Unlift m), Monad m) => MonadUnliftable m where
    type Unlift m :: * -> *
    withUnlift :: ((forall a. m a -> Unlift m a) -> Unlift m r) -> m r

instance MonadUnliftable IO where
    type Unlift IO = IO
    withUnlift k = k id

instance MonadUnliftable m => MonadUnliftable (ReaderT r m) where
    type Unlift (ReaderT r m) = Unlift m
    withUnlift k = ReaderT $ \r -> withUnlift $ \unlift -> k $ unlift . flip runReaderT r

instance MonadUnliftable m => MonadUnliftable (ExceptT e m) where
    type Unlift (ExceptT e m) = ExceptT e (Unlift m)
    withUnlift k = ExceptT $ withUnlift $ \unlift -> runExceptT $ k $ ExceptT . unlift . runExceptT

type MonadUnlift b m = (MonadUnliftable m, Unlift m ~ b)

liftU :: MonadUnlift b m => b a -> m a
liftU a = withUnlift $ \_ -> a

printM :: (MonadIO m, Show a) => a -> m ()
printM = liftIO . print

forkU :: MonadUnlift IO m => m () -> m ThreadId
forkU a = withUnlift $ \unlift -> forkIO $ unlift a

throwErrorU :: (MonadUnlift b m, MonadError e b) => e -> m a
throwErrorU = liftU . throwError

catchErrorU :: (MonadUnlift b m, MonadError e b) => m a -> (e -> m a) -> m a
a `catchErrorU` f = withUnlift $ \unlift -> unlift a `catchError` (unlift . f)

newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable)

testApp :: ExceptT () App ()
testApp = throwError () `catchError` \() -> do
    _ <- lift . forkU $ printM ()
    printM ()

testAppU :: ExceptT () App ()
testAppU = throwErrorU () `catchErrorU` \() -> do
    _ <- lift . forkU $ printM ()
    printM ()

newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable, MFunctor)

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwErrorU () `catchErrorU` \() -> do
    _ <- hoist lift . forkU $ printM ()
    printM ()
