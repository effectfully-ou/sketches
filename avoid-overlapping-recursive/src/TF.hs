{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module TF where

import           GHC.Exts (Proxy#, proxy#)

data N = Z | S N

type family Domain ab where
    Domain (a -> _) = a

type family Codomain ab where
    Codomain (_ -> b) = b

type family AppFunc f n a where
    AppFunc f  'Z    a = f a
    AppFunc f ('S n) a = f (Domain a) -> AppFunc f n (Codomain a)

class Applyable a n where
    apply :: Applicative f => Proxy# n -> f a -> AppFunc f n a

instance Applyable a 'Z where
    apply _ = id

instance (ab ~ (a -> b), Applyable b n) => Applyable ab ('S n) where
    apply _ f = \a -> apply (proxy# :: Proxy# n) $ f <*> a

liftAn
    :: forall n f a b. (Applyable b n, Applicative f)
    => (a -> b) -> f a -> AppFunc f n b
liftAn f = apply (proxy# :: Proxy# n) . fmap f



test1 :: (Enum a, Num a) => [a]
test1 = liftAn @'Z succ [1..5]

test2 :: (Enum a, Num a) => [a]
test2 = liftAn @('S 'Z) (+) [1..5] [3..5]

test3 :: (Enum a, Num a) => [a]
test3 = liftAn @('S ('S 'Z)) (\x y z -> x + y * z) [1..5] [3..5] [5]
