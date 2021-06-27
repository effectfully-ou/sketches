{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module UnliftPeel where

import           SomeAction

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Debug.Trace

class (Monad b, Monad m) => MonadUnliftPeel p b m | m b -> p, m p -> b where
    withUnliftPeel :: ((forall a. p a -> b a) -> b r) -> m r

instance MonadUnliftPeel IO IO IO where
    withUnliftPeel k = k id

instance MonadUnliftPeel p b m => MonadUnliftPeel (ReaderT e p) b (ReaderT e m) where
    withUnliftPeel k = ReaderT $ \r -> withUnliftPeel $ \unlift -> k $ unlift . flip runReaderT r

instance {-# OVERLAPPABLE #-} MonadUnliftPeel p b m => MonadUnliftPeel p b (ExceptT e m) where
    withUnliftPeel k =
        trace "Dropping ExceptT" $
            lift $ withUnliftPeel k

instance {-# OVERLAPPING #-}
            MonadUnliftPeel p b m => MonadUnliftPeel (ExceptT e p) (ExceptT e b) (ExceptT e m) where
    withUnliftPeel k =
        trace "Keeping ExceptT" $
            ExceptT $ withUnliftPeel $ \unlift -> runExceptT $ k $ mapExceptT unlift

liftU :: MonadUnliftPeel m b m => b r -> m r
liftU a = withUnliftPeel $ \_ -> a

forkU :: MonadUnliftPeel p IO m => p () -> m ThreadId
forkU a = withUnliftPeel $ \unlift -> forkIO $ unlift a

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

runExceptTApp :: ExceptT () App a -> IO ()
runExceptTApp = void . flip runReaderT () . unApp . runExceptT

-- >>> runExceptTApp testApp1
-- Keeping ExceptT
-- Keeping ExceptT
-- performed some action
-- ()
testApp1 :: ExceptT () App ()
testApp1 = throwErrorU () `catchErrorU` \() -> do
    _ <- lift $ forkU someAction
    printM ()

-- >>> runExceptTApp testApp2
-- Keeping ExceptT
-- Keeping ExceptT
-- Dropping ExceptT
-- performed some action
-- ()
testApp2 :: ExceptT () App ()
testApp2 = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU someAction
    printM ()

newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving newtype (Functor, Applicative, Monad, MonadIO)
      deriving anyclass (SomeAction)

instance MonadUnliftPeel p b m => MonadUnliftPeel (AppT p) b (AppT m) where
    withUnliftPeel k = AppT $ withUnliftPeel $ \unlift -> k $ unlift . unAppT

runAppTExcepT :: AppT (ExceptT () IO) a -> IO ()
runAppTExcepT = void . runExceptT . flip runReaderT () . unAppT

-- >>> runAppTExcepT testAppT
-- Keeping ExceptT
-- Keeping ExceptT
-- Dropping ExceptT
-- performed some action
-- ()
testAppT :: AppT (ExceptT () IO) ()
testAppT = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU someAction
    printM ()

testAppG
    :: ( MonadUnliftPeel m b m, MonadError () b
       , MonadUnliftPeel p IO m
       , SomeAction p
       , MonadIO m
       )
    => m ()
testAppG = throwErrorU () `catchErrorU` \() -> do  -- MonadUnliftPeel m b m, MonadError () b
    _ <-
        forkU                                      -- MonadUnliftPeel p IO m
            someAction                             -- SomeAction p
    printM ()                                      -- MonadIO m

testApp2G :: ExceptT () App ()
testApp2G = testAppG

testAppTG :: AppT (ExceptT () IO) ()
testAppTG = testAppG
