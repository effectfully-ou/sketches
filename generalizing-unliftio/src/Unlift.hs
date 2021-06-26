{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Unlift where

import           SomeAction

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.ST
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Reader
import           Data.Functor.Identity

-- class (Monad b, Monad m) => MonadUnlift b m | m -> b where
--     withUnlift :: ((forall a. m a -> b a) -> b r) -> m r

class (Monad (Unlift m), Monad m) => MonadUnliftable m where
    type Unlift m :: * -> *
    withUnlift :: ((forall a. m a -> Unlift m a) -> Unlift m r) -> m r

instance MonadUnliftable IO where
    type Unlift IO = IO
    withUnlift k = k id

instance MonadUnliftable m => MonadUnliftable (ReaderT r m) where
    type Unlift (ReaderT r m) = Unlift m
    withUnlift k = ReaderT $ \r -> withUnlift $ \unlift -> k $ unlift . flip runReaderT r

instance MonadUnliftable (ST s) where
    type Unlift (ST s) = ST s
    withUnlift k = k id

instance MonadUnliftable Identity where
    type Unlift Identity = Identity
    withUnlift k = k id

instance MonadUnliftable m => MonadUnliftable (IdentityT m) where
    type Unlift (IdentityT m) = m
    withUnlift k = IdentityT $ k runIdentityT

instance MonadUnliftable m => MonadUnliftable (ExceptT e m) where
    type Unlift (ExceptT e m) = ExceptT e (Unlift m)
    withUnlift k = ExceptT $ withUnlift $ \unlift -> runExceptT $ k $ mapExceptT unlift

type MonadUnlift b m = (MonadUnliftable m, Unlift m ~ b)

liftU :: MonadUnlift b m => b a -> m a
liftU a = withUnlift $ \_ -> a

forkU :: MonadUnlift IO m => m () -> m ThreadId
forkU a = withUnlift $ \unlift -> forkIO $ unlift a

throwErrorU :: (MonadUnlift b m, MonadError e b) => e -> m a
throwErrorU = liftU . throwError

catchErrorU :: (MonadUnlift b m, MonadError e b) => m a -> (e -> m a) -> m a
a `catchErrorU` f = withUnlift $ \unlift -> unlift a `catchError` (unlift . f)

newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable)
      deriving anyclass (SomeAction)

testApp :: ExceptT () App ()
testApp = throwError () `catchError` \() -> do
    _ <- lift $ forkU someAction
    printM ()

testAppU :: ExceptT () App ()
testAppU = throwErrorU () `catchErrorU` \() -> do
    _ <- lift $ forkU someAction
    printM ()

newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftable, MFunctor)
      deriving anyclass (SomeAction)

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwErrorU () `catchErrorU` \() -> do
    _ <- hoist lift $ forkU someAction
    printM ()
