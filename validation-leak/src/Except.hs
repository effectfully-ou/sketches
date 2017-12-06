{-# LANGUAGE FunctionalDependencies #-}
module Except where

class Except f e | f -> e where
  throw :: e -> f a
