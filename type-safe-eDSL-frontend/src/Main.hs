{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import GHC.TypeLits
import GHC.Records
import GHC.Exts
import Data.Kind
import Unsafe.Coerce (UnsafeEquality (..), unsafeEqualityProof)

main = undefined

type Prefix :: Symbol -> Type -> Type
data Prefix prefix a = Prefix

type TypeOf :: Symbol -> Type
type family TypeOf var

data Var name a where
    Var :: forall a name. TypeOf name ~ a => Var name a

data DeclTypeOf name a where
    DeclInt :: TypeOf name ~ Int => DeclTypeOf name Int
    DeclBool :: TypeOf name ~ Bool => DeclTypeOf name Bool

class KnownType a where
    knownType :: forall name. TypeOf name ~ a => DeclTypeOf name a
instance KnownType Int where
    knownType = DeclInt
instance KnownType Bool where
    knownType = DeclBool

type Equals :: forall a. a -> a -> Bool
type family Equals x y where
    Equals x x = 'True
    Equals _ _ = 'False

type UnequatableGo :: Bool -> ErrorMessage -> Constraint
class UnequatableGo eq msg
instance {-# INCOHERENT #-} UnequatableGo eq msg
instance TypeError msg => UnequatableGo 'True  msg
instance TypeError msg => UnequatableGo 'False msg

type Unequatable :: ErrorMessage -> Type -> Type -> Constraint
type Unequatable msg a b = UnequatableGo (Equals a b) msg

-- The 'Unequatable' constraint prevents @declare.name@ from being used for the same @name@
-- twice. The way it works is that upon a subsequent call to @declare.name@ the user either supplies
-- the same type for @name@ triggering the @UnequatableGo 'True msg@ instance or a different one
-- triggering the @UnequatableGo 'False msg@ instance and both of those throw a
-- 'TypeError'. I.e. this instance only type checks successfully when GHC can't tell whether @TypeOf
-- name@ is equal to @a@ or not, which is only the case when @declare.name@ is invoked exactly once.
--
-- TODO: would be nice to make pattern matching on the type tag mandatory, because without the
-- matching a lot guarantees go out of the window... or do they, given that the forged equality
-- constraint is only accessible through pattern matching?
instance
        ( res ~ (Expr a -> Body (Var name a)), KnownSymbol name
        , Unequatable ('Text "'" :<>: 'Text name :<>: 'Text "' is declared twice") (TypeOf name) a
        ) => HasField name (Prefix "declare" a) res where
    getField _ (Expr expr_) = do
        let name = symbolVal' (proxy# @name)
        statement $ DeclVariable name expr_
        -- It would be very dangerous to allow for forging multiple @TypeOf name ~ a1@,
        -- @TypeOf name ~ a2@ etc constraints, since GHC could conclude @a1 ~ a2@ from those,
        -- which for all intents and purposes would be implicit uncontrolled 'unsafeCoerce', but
        -- declaring the same name twice is supposed to result in a 'TypeError', so this danger
        -- should be ruled out.
        case unsafeEqualityProof @(TypeOf name) @a of
            UnsafeRefl -> pure Var

instance (res ~ Expr a, TypeOf name ~ a, KnownSymbol name, KnownType a) =>
        HasField name (Prefix "get" a) res where
    getField _ = Expr $ case knownType @a @name of
        DeclInt -> VarF name
        DeclBool -> VarB name
      where
        name = symbolVal' (proxy# @name)

-- 'KnownType' is only needed to prevent @set.x@ from being successfully type checked when there's
-- no @x@ in the current scope. Without the constraint, GHC would happily infer
--
-- > set.x :: Expr (TypeOf "x") -> Body ()
instance (res ~ (Expr a -> Body ()), TypeOf name ~ a, KnownSymbol name, KnownType a) =>
        HasField name (Prefix "set" a) res where
    getField _ (Expr expr_) = do
        let name = symbolVal' (proxy# @name)
        statement $ AssignVar name expr_

instance (res ~ ((Expr a -> Expr a) -> Body ()), TypeOf name ~ a, KnownSymbol name, KnownType a) =>
        HasField name (Prefix "mut" a) res where
    getField _ f = getField @name set . f $ getField @name get

newtype Mut a = Mut a
instance res ~ Mut (Expr a -> r) => HasField "mut" (Expr a -> r) res where
    getField = Mut

-- TODO: generalize to arbitrary arity.
instance (a ~ a', res ~ (Expr b -> Body ()), TypeOf name ~ a, KnownSymbol name, KnownType a) =>
        HasField name (Mut (Expr a -> Expr b -> Expr a')) res where
    getField (Mut f) y = getField @name mut $ \x -> f x y

declare :: Prefix "declare" a
declare = Prefix

get :: Prefix "get" a
get = Prefix

set :: Prefix "set" a
set = Prefix

mut :: Prefix "mut" a
mut = Prefix
