{-# LANGUAGE ConstraintKinds            #-}
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

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Debug.Trace

class (Monad b, Monad m) => MonadUnliftPeel p b m | m -> p, m -> b where
    withUnliftPeel :: ((forall a. p a -> b a) -> b r) -> m r

instance MonadUnliftPeel IO IO IO where
    withUnliftPeel k = k id

instance MonadUnliftPeel p b m => MonadUnliftPeel (ReaderT e p) b (ReaderT e m) where
    withUnliftPeel k = ReaderT $ \r -> withUnliftPeel $ \unlift -> k $ unlift . flip runReaderT r

instance {-# OVERLAPPABLE #-} MonadUnliftPeel p b m => MonadUnliftPeel p b (ExceptT e m) where
    withUnliftPeel k =
        trace "OVERLAPPABLE" $
            lift $ withUnliftPeel k

instance {-# OVERLAPPING #-}
            MonadUnliftPeel p b m => MonadUnliftPeel (ExceptT e p) (ExceptT e b) (ExceptT e m) where
    withUnliftPeel k =
        trace "OVERLAPPING" $
            ExceptT $ withUnliftPeel $ \unlift -> runExceptT $ k $ ExceptT . unlift . runExceptT

liftU :: MonadUnliftPeel p b m => b r -> m r
liftU a = withUnliftPeel $ \_ -> a

printM :: (MonadIO m, Show a) => a -> m ()
printM = liftIO . print

forkU :: MonadUnliftPeel p IO m => p () -> m ThreadId
forkU a = withUnliftPeel $ \unlift -> forkIO $ unlift a

throwErrorU :: (MonadUnliftPeel m b m, MonadError e b) => e -> m a
throwErrorU = liftU . throwError

catchErrorU :: (MonadUnliftPeel m b m, MonadError e b) => m a -> (e -> m a) -> m a
catchErrorU a f = withUnliftPeel $ \unlift -> unlift a `catchError` (unlift . f)

newtype App a = App
    { unApp :: ReaderT () IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadUnliftPeel App IO App where
    withUnliftPeel k = App $ withUnliftPeel $ \unlift -> k $ unlift . unApp

runExceptTApp :: ExceptT () App a -> IO ()
runExceptTApp = void . flip runReaderT () . unApp . runExceptT

-- >>> runExceptTApp testApp1
-- OVERLAPPING
-- OVERLAPPING
-- ()
-- ()
testApp1 :: ExceptT () App ()
testApp1 = throwErrorU () `catchErrorU` \() -> do
    _ <- lift . forkU $ printM ()
    printM ()

-- >>> runExceptTApp testApp2
-- OVERLAPPING
-- OVERLAPPING
-- OVERLAPPABLE
-- ()
-- ()
testApp2 :: ExceptT () App ()
testApp2 = throwErrorU () `catchErrorU` \() -> do
    _ <- forkU $ printM ()
    printM ()

newtype AppT m a = AppT
    { unAppT :: ReaderT () m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadError e)

instance MonadUnliftPeel p b m => MonadUnliftPeel (AppT p) b (AppT m) where
    withUnliftPeel k = AppT $ withUnliftPeel $ \unlift -> k $ unlift . unAppT

testAppT :: AppT (ExceptT () IO) ()
testAppT = throwError () `catchError` \() -> do
    _ <- forkU $ printM ()
    printM ()

-- testAppG = throwErrorU () `catchErrorU` \() -> do
--     _ <- forkU $ printM ()
--     printM ()
