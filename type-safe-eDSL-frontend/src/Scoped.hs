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

type FreeVariableError :: Symbol -> Constraint
type family FreeVariableError name where
    FreeVariableError name =
        TypeError ('Text "Can't reference a free variable: ‘" :<>: 'Text name :<>: 'Text "’")

type ThrowOnFree :: Constraint -> () -> Constraint
type family ThrowOnFree err scoped where
    ThrowOnFree _   '() = ()
    ThrowOnFree err  _  = err

instance (res ~ Term, KnownSymbol name, ThrowOnFree (FreeVariableError name) (IsScoped name)) =>
        HasField name (Prefix "var") res where
    getField _ = Var $ symbolVal' (proxy# @name)

-- >>> print owl
-- Lam "f" (Lam "g" (App (Var "g") (App (Var "f") (Var "g"))))
owl :: Term
owl = lam.f $ \Enter -> lam.g $ \Enter -> app var.g (app var.f var.g)

-- -- error: [GHC-64725]
-- --     • Can't reference a free variable: ‘x’
-- --     • In the expression: var.x
-- --       In an equation for ‘free’: free = var.x
-- free :: Term
-- free = var.x
