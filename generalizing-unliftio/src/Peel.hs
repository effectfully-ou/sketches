{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Peel where

import           Unlift

import           Control.Concurrent
import           Control.Monad.Except
import           Control.Monad.Trans.Reader

class (MonadUnliftable (Peel m), Monad m) => MonadPeelable m where
    type Peel m :: * -> *
    withPeel :: ((forall a. Peel m a -> Unlift (Peel m) a) -> Unlift (Peel m) b) -> m b

instance MonadPeelable IO where
    type Peel IO = IO
    withPeel = withUnlift

instance MonadPeelable m => MonadPeelable (ReaderT r m) where
    type Peel (ReaderT r m) = ReaderT r (Peel m)
    withPeel k = ReaderT $ \r -> withPeel $ \peel -> k $ peel . flip runReaderT r

instance MonadPeelable m => MonadPeelable (ExceptT e m) where
    type Peel (ExceptT e m) = Peel m
    withPeel k = lift $ withPeel k

type MonadPeel p m = (MonadPeelable m, Peel m ~ p)

forkP :: (MonadPeel p m, MonadUnlift IO p) => p () -> m ThreadId
forkP a = withPeel $ \peel -> forkIO $ peel a

instance MonadPeelable App where
    type Peel App = App
    withPeel k = App $ withPeel $ \peel -> k $ peel . unApp

instance MonadPeelable m => MonadPeelable (AppT m) where
    type Peel (AppT m) = AppT (Peel m)
    withPeel k = AppT $ withPeel $ \peel -> k $ peel . unAppT

testAppG
    :: ( MonadUnlift b m, MonadError () b
       , MonadPeel p m, MonadUnlift IO p
       , MonadIO p
       , MonadIO m
       )
    => m ()
testAppG = throwErrorU () `catchErrorU` \() -> do  -- MonadUnlift b m, MonadError () b
    _ <- forkP $                                   -- MonadPeel p m, MonadUnliftIO p
        printM ()                                  -- MonadIO p
    printM ()                                      -- MonadIO m

testApp :: ExceptT () App ()
testApp = testAppG

testAppT :: AppT (ExceptT () IO) ()
testAppT = testAppG
