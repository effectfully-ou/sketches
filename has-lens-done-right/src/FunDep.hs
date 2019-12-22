{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module FunDep where

import           Control.Lens hiding (lens)
import           Data.Proxy
import           Data.Tagged
import           Data.Void
import           GHC.Exts     (Proxy#, proxy#)

-- Core
--------------------

class HasLens (x :: k) s t a b | x s -> a, x t -> b, x s b -> t, x t a -> s where
    lensAt :: Proxy# x -> Lens s t a b

lens :: forall x s t a b. HasLens x s t a b => Lens s t a b
lens = lensAt @x proxy#

-- The 'User' example with good type inference
--------------------

data User = User
    { userEmail :: String
    , userName  :: String
    }

instance (a ~ String, b ~ String) => HasLens "name" User User a b where
    lensAt _ f (User email name) = User email <$> f name

-- Found hole: _ :: [Char] -> [Char]
--
-- test = User "john@gmail.com" "John" & lens @"name" %~ _

-- Found type wildcard ‘_’ standing for ‘([Char] -> [Char]) -> User’
test0 :: _
test0 f = User "john@gmail.com" "John" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘User’
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"

-- The 'User' example with bad type inference
--------------------

instance HasLens "nameBad" User User String String where
    lensAt _ f (User email name) = User email <$> f name

-- Found hole: _ :: String -> b1
-- Where: ‘b1’ is an ambiguous type variable
--
-- test' = User "john@gmail.com" "John" & lens @"nameBad" %~ _

-- Found type wildcard ‘_’ standing for ‘(String -> b1) -> b0’
-- Where: ‘b1’ is an ambiguous type variable
--        ‘b0’ is an ambiguous type variable
--
-- Also throws an error:
--
-- Ambiguous type variables ‘b0’, ‘b1’ arising from a use of ‘lens’
-- prevents the constraint ‘(HasLens "nameBad" User b0 String b1)’ from being solved.
--
-- test0' :: _
-- test0' f = User "john@gmail.com" "John" & lens @"nameBad" %~ f

-- Found type wildcard ‘_’ standing for ‘s0’
-- Where: ‘s0’ is an ambiguous type variable
--
-- Also throws an error:
--
-- Ambiguous type variables ‘s0’, ‘a0’ arising from a use of ‘lens’
-- prevents the constraint ‘(HasLens "nameBad" s0 User a0 [Char])’ from being solved.
--
-- test1' :: _ -> User
-- test1' user = user & lens @"nameBad" .~ "new name"

data NamelessGod = NamelessGod
    { namelessGodEmail :: String
    }

instance HasLens "nameBad" User NamelessGod String () where
    lensAt _ f (User email name) = NamelessGod email <$ f name

apotheosis :: NamelessGod
apotheosis = User "john@gmail.com" "John" & lens @"nameBad" .~ ()

instance HasLens "nameBad" User Void String Void where
    lensAt _ f (User _ name) = f name

type Deathnote = String -> Void

write :: Deathnote -> User -> Void
write kill user = user & lens @"nameBad" %~ kill

-- Tuple examples
--------------------

instance HasLens "_1" (a, b) (a', b) a a' where
    lensAt _ = _1

-- Found type wildcard ‘_’ standing for ‘((a, Char), Bool)’
test2 :: forall a. (Enum a, Num a) => _
test2 = ((0 :: a, 'a'), True) & lens @"_1" . lens @"_1" %~ succ

-- Phantoms do not just work
--------------------

data Ph (x :: k) (bs :: [Bool]) = Ph { foo :: Int }

-- Illegal instance declaration for
--   ‘HasLens "foo" (Ph x bs) (Ph x' bs') a b’
--   The liberal coverage condition fails in class ‘HasLens’
--     for functional dependency: ‘x s b -> t’
--   Reason: lhs types ‘"foo"’, ‘Ph x bs’, ‘b’
--     do not jointly determine rhs type ‘Ph x' bs'’
--   Un-determined variables: k', x', bs'
--
-- instance (a ~ Int, b ~ Int) => HasLens "foo" (Ph (x :: k) bs) (Ph (x' :: k') bs') a b where
--     lensAt _ f (Ph i) = Ph <$> f i

-- Type families do not just work
--------------------

type family Goo (x :: k)
data Tf (x :: k) = Tf { bar :: Goo x }

-- Illegal instance declaration for
--   ‘HasLens "foo" (Tf x) (Tf x') a b’
--   The liberal coverage condition fails in class ‘HasLens’
--     for functional dependency: ‘x s b -> t’
--   Reason: lhs types ‘"bar"’, ‘Tf x’, ‘b’
--     do not jointly determine rhs type ‘Tf x'’
--   Un-determined variables: k', x'
--
-- instance (a ~ Goo x, b ~ Goo x') => HasLens "bar" (Tf (x :: k)) (Tf (x' :: k')) a b where
--     lensAt _ f (Tf x) = Tf <$> f x

-- But with a bit of hacking phantoms do work
--------------------

instance (a ~ Tagged ('(,) x bs) Int, b ~ Tagged ('(,) x' bs') Int) =>
            HasLens "foo" (Ph (x :: k) bs) (Ph (x' :: k') bs') a b where
    lensAt _ f (Ph i) = Ph . unTagged <$> f (Tagged i)

ph :: Lens (Ph (a :: k) bs) (Ph (a' :: k') bs') Int Int
ph = lens @"foo" . coerced

-- As well as type families
--------------------

instance (a ~ Tagged x (Goo x), b ~ Tagged x' (Goo x')) =>
            HasLens "bar" (Tf (x :: k)) (Tf (x' :: k')) a b where
    lensAt _ f (Tf x) = Tf . unTagged <$> f (Tagged x)

tf :: Lens (Tf (a :: k)) (Tf (a' :: k')) (Goo a) (Goo a')
tf = lens @"bar" . coerced
