{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Unscoped where

import Common

import GHC.TypeLits
import GHC.Records
import GHC.Exts

instance (res ~ (Term -> Term), KnownSymbol name) => HasField name (Prefix "lam") res where
    getField _ = Lam $ symbolVal' (proxy# @name)

instance (res ~ Term, KnownSymbol name) => HasField name (Prefix "var") res where
    getField _ = Var $ symbolVal' (proxy# @name)

-- >>> print owl
-- Lam "f" (Lam "g" (App (Var "g") (App (Var "f") (Var "g"))))
owl :: Term
owl = lam.f $ lam.g $ app var.g (app var.f var.g)
