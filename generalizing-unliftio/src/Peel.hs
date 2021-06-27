{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Peel where

import           SomeAction
import           Unlift

import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Trans.Reader
import           Data.Functor.Identity

-- -- We could also always unlift whenever we peel, it's not clear to me what the trade-offs are.

-- class (MonadUnliftable (Peel m), Monad m) => MonadPeelable m where
--     type Peel m :: * -> *
--     withPeel :: ((forall a. Peel m a -> Unlift (Peel m) a) -> Unlift (Peel m) r) -> m r

-- forkP :: (MonadPeel p m, MonadUnlift IO p) => p () -> m ThreadId
-- forkP a = withPeel $ \peel -> forkIO $ peel a

class (Monad (Peel m), Monad m) => MonadPeelable m where
    type Peel m :: * -> *
    liftP :: Peel m a -> m a

instance MonadPeelable IO where
    type Peel IO = IO
    liftP = id

instance MonadPeelable Identity where
    type Peel Identity = Identity
    liftP = id

instance MonadPeelable m => MonadPeelable (ReaderT r m) where
    type Peel (ReaderT r m) = ReaderT r (Peel m)
    liftP = hoist liftP

instance MonadPeelable m => MonadPeelable (ExceptT e m) where
    type Peel (ExceptT e m) = Peel m
    liftP = lift . liftP

type MonadPeel p m = (MonadPeelable m, Peel m ~ p)

instance MonadPeelable App where
    type Peel App = App
    liftP = id

instance MonadPeelable m => MonadPeelable (AppT m) where
    type Peel (AppT m) = AppT (Peel m)
    liftP = hoist liftP

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

testApp :: ExceptT () App ()
testApp = testAppG

testAppT :: AppT (ExceptT () IO) ()
testAppT = testAppG
