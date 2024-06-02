{-# LANGUAGE StandaloneKindSignatures #-}

module Common where

import GHC.TypeLits
import Data.Kind

data Term
    = Var String
    | Lam String Term
    | App Term Term
    deriving (Show)

type Prefix :: Symbol -> Type
data Prefix prefix = Prefix
