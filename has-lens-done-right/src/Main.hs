{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}

module Main where

import           Control.Lens hiding (lens)
import           Data.Kind    (Type)
import           GHC.Prim

main :: IO ()
main = mempty

-- Core
--------------------

type family Get (x :: k) s

class SameModulo (x :: k) s t where
    lensAt :: (SameModulo x t s, a ~ Get x s, b ~ Get x t) => Proxy# x -> Lens s t a b

class (SameModulo x s t, SameModulo x t s, a ~ Get x s, b ~ Get x t) => HasLens x s t a b
instance (SameModulo x s t, SameModulo x t s, a ~ Get x s, b ~ Get x t) => HasLens x s t a b

type HasLens' x s a = HasLens x s s a a

lens :: forall x s t a b. HasLens x s t a b => Lens s t a b
lens = lensAt @x proxy#

lens' :: forall x s a. HasLens' x s a => Lens' s a
lens' = lens @x

-- The 'User' example
--------------------

data User = User
    { userEmail :: String
    , userName  :: String
    }

type instance Get "name" User = String
instance t ~ User => SameModulo "name" User t where
    lensAt _ f (User email name) = User email <$> f name

-- Found type wildcard ‘_’ standing for ‘([Char] -> String) -> User’
test0 :: _
test0 f = User "email" "name" & lens @"name" %~ f

-- Found type wildcard ‘_’ standing for ‘User’
test1 :: _ -> User
test1 user = user & lens @"name" .~ "new name"

-- Tuple examples
--------------------

type instance Get "_1" (a, b) = a
instance t ~ (a', b) => SameModulo "_1" (a, b) t where
    lensAt _ = _1

type instance Get "_1" (a, b, c) = a
instance t ~ (a', b, c) => SameModulo "_1" (a, b, c) t where
    lensAt _ = _1

-- Found type wildcard ‘_’ standing for ‘((a, Char), Bool)’
test2 :: forall a. (Enum a, Num a) => _
test2 = ((0 :: a, 'a'), True) & lens @"_1" . lens @"_1" %~ succ

mono :: (HasLens' "_1" s sa, HasLens' "_1" sa a) => Lens' s a
mono = lens' @"_1" . lens' @"_1"

monoTuple :: Lens' ((a, b), c, d) a
monoTuple = mono

-- Inlined version also works.
monoTupleInlined :: Lens' ((a, b), c, d) a
monoTupleInlined = lens @"_1" . lens @"_1"

poly
    :: (HasLens "_1" s t sa tb, HasLens "_1" sa tb a b)
    => Lens s t a b
poly = lens @"_1" . lens @"_1"

polyTuple :: Lens ((a, b), c, d) ((a', b), c, d) a a'
polyTuple = poly

-- Inlined version also works.
polyTupleInlined :: Lens ((a, b), c, d) ((a', b), c, d) a a'
polyTupleInlined = lens @"_1" . lens @"_1"

-- Found type wildcard ‘_’ standing for ‘((Int, Bool), Char)’
polyTupleTest :: _
polyTupleTest = (("abc", True), 'd') & lens @"_1" . lens @"_1" %~ length

-- The phantom arguments problem is solved (https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-phantom-arguments):

data Ph (a :: k) (bs :: [Bool]) = Ph { foo :: Int }

type instance Get "foo" (Ph a b) = Int
instance t ~ Ph (a' :: k') bs' => SameModulo "foo" (Ph a b) t where
    lensAt _ f (Ph i) = Ph <$> f i

ph :: Lens (Ph (a :: k) bs) (Ph (a' :: k') bs') Int Int
ph = lens @"foo"

-- The type families problem is solved (https://gitlab.haskell.org/ghc/ghc/wikis/records/overloaded-record-fields/design#type-changing-update-type-families):
--------------------

type family Goo (a :: k)
data Tf (a :: k) = Tf { bar :: Goo a }

type instance Get "bar" (Tf a) = Goo a
instance t ~ Tf (a' :: k') => SameModulo "bar" (Tf (a :: k)) t where
    lensAt _ f (Tf x) = Tf <$> f x

tf :: Lens (Tf (a :: k)) (Tf (a' :: k')) (Goo a) (Goo a')
tf = lens @"bar"
