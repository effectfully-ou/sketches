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

import           Data.Void
import           GHC.Exts   (Proxy#, proxy#)
import           Lens.Micro hiding (lens)

-- Core
--------------------

class HasLens (x :: k) s t a b | x s -> a, x t -> b, x s b -> t, x t a -> s where
    lensAt :: Proxy# x -> Lens s t a b

instance HasLens "_1" (a, b) (a', b) a a' where
    lensAt _ = _1

instance HasLens "_1" (a, b, c) (a', b, c) a a' where
    lensAt _ = _1

lens :: forall x s t a b. HasLens x s t a b => Lens s t a b
lens = lensAt @x proxy#

-- The 'User' example
--------------------

data User = User
    { userEmail :: String
    , userName  :: String
    }

instance HasLens "name" User User String String where
    lensAt _ f (User email name) = User email <$> f name

-- Found hole: _ :: String -> b1
-- Where: ‘b1’ is an ambiguous type variable
test = User "john@gmail.com" "John" & lens @"name" %~ _

-- Found type wildcard ‘_’ standing for ‘(String -> b1) -> b0’
-- Where: ‘b1’ is an ambiguous type variable
--        ‘b0’ is an ambiguous type variable
--
-- Also throws an error:
--
-- Ambiguous type variables ‘b0’, ‘b1’ arising from a use of ‘lens’
-- prevents the constraint ‘(HasLens "name" User b0 String b1)’ from being solved.
test0 :: _
test0 f = User "john@gmail.com" "John" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘s0’
-- Where: ‘s0’ is an ambiguous type variable
-- Also throws an error:
--
-- Ambiguous type variables ‘s0’, ‘a0’ arising from a use of ‘lens’
-- prevents the constraint ‘(HasLens "name" s0 User a0 [Char])’ from being solved.
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"

data NamelessGod = NamelessGod
    { namelessGodEmail :: String
    }

instance HasLens "name" User NamelessGod String () where
    lensAt _ f (User email name) = NamelessGod email <$ f name

apotheosis :: NamelessGod
apotheosis = User "john@gmail.com" "John" & lens @"name" .~ ()

instance HasLens "name" User Void String Void where
    lensAt _ f (User _ name) = f name

type Deathnote = String -> Void

write :: Deathnote -> User -> Void
write kill user = user & lens @"name" %~ kill
