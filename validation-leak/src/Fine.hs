{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Fine where

import Data.Monoid
import Except

data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
  fmap f (Failure e) = Failure  e
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success

  Failure e1 <*> b = Failure $ e1 `mappend` case b of
    Failure e2 -> e2
    Success _  -> mempty
  Success _  <*> Failure e  = Failure  e
  Success f  <*> Success x  = Success (f x)

instance Except (Validation e) e where
  throw = Failure
