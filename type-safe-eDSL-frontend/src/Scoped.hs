{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Scoped where

import Common

import GHC.TypeLits
import GHC.Records
import GHC.Exts
import Unsafe.Coerce (UnsafeEquality (..), unsafeEqualityProof)

type IsScoped :: Symbol -> ()
type family IsScoped name

type InScope name = IsScoped name ~ '()

data Enter name where
    Enter :: InScope name => Enter name

-- @res ~ (InScope name => Term -> Term)@ doesn't work.
instance (res ~ ((Enter name -> Term) -> Term), KnownSymbol name) =>
        HasField name (Prefix "lam") res where
    getField _ k =
        case unsafeEqualityProof @(IsScoped name) @'() of
            UnsafeRefl -> Lam (symbolVal' (proxy# @name)) $ k Enter

instance (res ~ Term, KnownSymbol name, InScope name) => HasField name (Prefix "var") res where
    getField _ = Var $ symbolVal' (proxy# @name)

var :: Prefix "var"
var = Prefix

lam :: Prefix "lam"
lam = Prefix

app :: Term -> Term -> Term
app = App

-- >>> print owl
-- Lam "f" (Lam "g" (App (Var "g") (App (Var "f") (Var "g"))))
owl :: Term
owl = lam.f $ \Enter -> lam.g $ \Enter -> app var.g (app var.f var.g)
