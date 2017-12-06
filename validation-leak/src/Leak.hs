{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Leak where

import Data.Semigroup
import Except

data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
  fmap f (Failure e) = Failure  e
  fmap f (Success x) = Success (f x)

instance Semigroup e => Applicative (Validation e) where
  pure = Success

  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e  <*> Success _  = Failure  e
  Success _  <*> Failure e  = Failure  e
  Success f  <*> Success x  = Success (f x)

instance Except (Validation e) e where
  throw = Failure
