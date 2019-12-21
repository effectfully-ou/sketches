{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TF where

import           Data.Proxy   (Proxy (..))
import           GHC.Exts     (Proxy#, proxy#)
import           GHC.TypeLits (Symbol)
import           Lens.Micro   hiding (lens)
import qualified Lens.Micro   as Lens (lens)

-- Core
--------------------

type family FldTy (r :: *) (n :: Symbol) :: *

class t ~ FldTy r n => Has r (n :: Symbol) t where
    getField :: Proxy# n -> r -> t

type family UpdTy (r :: *) (n :: Symbol) (a :: *) :: *

class (Has r n (FldTy r n), r ~ UpdTy r n (FldTy r n)) =>
            Upd (r :: *) (n :: Symbol) (t :: *) where
    setField :: Proxy# n -> r -> t -> UpdTy r n t

lens
    :: forall n s t a b. (Upd s n b, t ~ UpdTy s n b, a ~ FldTy s n)
    => Lens s t a b
lens = Lens.lens (getField pn) (setField pn) where
    pn :: Proxy# n
    pn = proxy#

-- The 'User' example
--------------------

data User = User
    { userEmail :: String
    , userName  :: String
    }

type instance FldTy User "name" = String
type instance UpdTy User "name" String = User

instance t ~ String => Has User "name" t where
    getField _ (User _ name) = name

instance t ~ String => Upd User "name" t where
    setField _ (User email _) name = User email name

-- Found type wildcard ‘_’ standing for ‘([Char] -> [Char]) -> User’
test0 :: _
test0 f = User "john@gmail.com" "John" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘s0’
-- Where: ‘s0’ is an ambiguous type variable
--
-- But also throws an error:
--
-- Couldn't match type ‘UpdTy s0 "name" [Char]’ with ‘User’ arising from a use of ‘lens’
-- The type variable ‘s0’ is ambiguous
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"

-- The wrong 'User' example
--------------------

data User' = User'
    { userEmail' :: String
    , userName'  :: String
    }

data NamelessGod = NamelessGod
    { namelessGodEmail :: String
    }

type instance FldTy User' "name" = String
type instance UpdTy User' "name" String = User'
type instance UpdTy User' "name" () = NamelessGod

instance t ~ String => Has User' "name" t where
    getField _ (User' _ name) = name

instance Upd User' "name" String where
    setField _ (User' email _) name = User' email name

instance Upd User' "name" () where
    setField _ (User' email _) () = NamelessGod email

-- Found type wildcard ‘_’
--   standing for ‘([Char] -> b0) -> UpdTy User' "name" b0’
-- Where: ‘b0’ is an ambiguous type variable
--
-- But also throws an error:
--
-- Ambiguous type variable ‘b0’ arising from a use of ‘lens’
-- prevents the constraint ‘(Upd User' "name" b0)’ from being solved.
test0' :: _
test0' f = User' "john@gmail.com" "John" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘NamelessGod’
apotheosis :: _
apotheosis = User' "john@gmail.com" "John" & lens @"name" .~ ()

-- Tuple examples
--------------------

type instance FldTy (a, b) "_1" = a
type instance UpdTy (a, b) "_1" a' = (a', b)

instance a ~ t => Has (a, b) "_1" t where
    getField _ (a, b) = a

instance Upd (a, b) "_1" t where
    setField _ (_, y) x' = (x', y)

-- Found type wildcard ‘_’ standing for ‘((a, Char), Bool)’
test2 :: forall a. (Enum a, Num a) => _
test2 = ((0 :: a, 'a'), True) & lens @"_1" . lens @"_1" %~ succ

-- Poly-kinded update works
--------------------

data UserK (x :: k) = UserK
    { nameK  :: String
    , proxyK :: Proxy x
    }

type instance FldTy (UserK (x :: k)) "proxyK" = Proxy x
type instance UpdTy (UserK (x :: k)) "proxyK" (Proxy (x' :: k')) = UserK x'

instance t ~ Proxy x => Has (UserK (x :: k)) "proxyK" t where
    getField _ (UserK _ proxy) = proxy

instance t ~ Proxy (x' :: k') => Upd (UserK (x :: k)) "proxyK" t where
    setField _ (UserK name _) proxy = UserK name proxy

-- Found type wildcard ‘_’ standing for ‘(Proxy "text" -> Proxy x') -> UserK x'’
test0K :: _
test0K f = UserK "john@gmail.com" (Proxy @"text") & lens @"proxyK" %~ f
