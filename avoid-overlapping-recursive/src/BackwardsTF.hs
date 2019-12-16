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

module BackwardsTF where

import           GHC.Exts (Proxy#, proxy#)

data N = Z | S N

type family UnAppFunc n a where
    UnAppFunc  'Z    (f b)      = b
    UnAppFunc ('S n) (f a -> b) = a -> UnAppFunc n b

type family CountArgs f where
    CountArgs (_ -> b) = 'S (CountArgs b)
    CountArgs  _       = 'Z

class Applyable (f :: * -> *) b n where
    apply :: Proxy# n -> f (UnAppFunc n b) -> b

instance b ~ f a => Applyable f b 'Z where
    apply _ = id

instance (ab ~ (f a -> b), Applyable f b n, Applicative f) => Applyable f ab ('S n) where
    apply _ f = \a -> apply (proxy# :: Proxy# n) $ f <*> a

liftAn
    :: forall n f a b. (n ~ CountArgs b, Applyable f b n, Applicative f)
    => (a -> UnAppFunc n b) -> f a -> b
liftAn f = apply (proxy# :: Proxy# n) . fmap f



test1 :: (Enum a, Num a) => [a]
test1 = liftAn succ [1..5]

test2 :: (Enum a, Num a) => [a]
test2 = liftAn (+) [1..5] [3..5]

test3 :: (Enum a, Num a) => [a]
test3 = liftAn (\x y z -> x + y * z) [1..5] [3..5] [5]

test4 :: (Enum a, Num a) => Maybe (Maybe a)
test4 = (liftAn . liftAn) (+) (Just (Just 0)) Nothing

test5 :: IO Int
test5 = do
    n <- liftAn (*) readLn readLn
    return $ n + 1
