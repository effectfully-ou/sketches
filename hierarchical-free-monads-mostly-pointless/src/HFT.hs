{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module HFT where

import           FT

import           Control.Applicative
import           System.Random

newtype Subsystem constr a = Subsystem
    { unSubsystem :: forall m. constr m => m a
    }

instance (forall f. constr f => Functor f) => Functor (Subsystem constr) where
    fmap f (Subsystem a) = Subsystem $ fmap f a
    x <$ (Subsystem a) = Subsystem $ x <$ a

instance (forall f. constr f => Applicative f) => Applicative (Subsystem constr) where
    pure x = Subsystem $ pure x
    Subsystem f <*> Subsystem a = Subsystem $ f <*> a
    Subsystem f <* Subsystem a = Subsystem $ f <* a
    Subsystem f *> Subsystem a = Subsystem $ f *> a
    liftA2 f (Subsystem a) (Subsystem b) = Subsystem $ liftA2 f a b

instance (forall m. constr m => Monad m) => Monad (Subsystem constr) where
    return = pure
    Subsystem a >>= f = Subsystem $ a >>= unSubsystem . f
    a >> b = a *> b
    fail err = Subsystem $ fail err

subsystem :: (forall m. constr2 m => constr1 m) => Subsystem constr1 a -> Subsystem constr2 a
subsystem (Subsystem a) = Subsystem a

instance (forall m. constr m => MonadLogger m) => MonadLogger (Subsystem constr) where
    logMessage level msg = Subsystem $ logMessage level msg

instance (forall m. constr m => MonadApp m) => MonadApp (Subsystem constr) where
    getRandomInt range = Subsystem $ getRandomInt range

type Logger = Subsystem MonadLogger
type App    = Subsystem MonadApp

runApp :: App a -> IO a
runApp = unSubsystem

logged :: Logger ()
logged = logMessage Info "a"

printRandomFactorial' :: App ()
printRandomFactorial' = do
    n <- getRandomInt (1, 100)
    logInfo $ show $ product [1..n]
    logMessage Info "b"
    subsystem logged
