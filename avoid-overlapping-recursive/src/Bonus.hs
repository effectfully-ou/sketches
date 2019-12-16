{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Bonus where

import           GHC.Exts (Proxy#, proxy#)

type family IsFun a :: Bool where
    IsFun (_ -> _) = 'True
    IsFun  _       = 'False

class DispatchApN f a b (ts :: Bool) where
    dispatchApN :: Proxy# ts -> f a -> b

instance f a ~ b => DispatchApN f a b 'False where
    dispatchApN _ = id

instance (Applicative f, xa ~ (x -> a), fxb ~ (f x -> b), ApN f a b) =>
            DispatchApN f xa fxb 'True where
    dispatchApN _ f = \a -> apN $ f <*> a

class Applicative f => ApN f a b where
    apN :: f a -> b

type family And (a :: Bool) (b :: Bool) :: Bool where
    And 'True 'True = 'True
    And _     _     = 'False

instance
        ( Applicative f
        , ifa ~ IsFun a, ifb ~ IsFun b, ifb ~ And ifa ifb
        , DispatchApN f a b ifb
        ) => ApN f a b where
    apN = dispatchApN (proxy# :: Proxy# ifb)

liftAn :: ApN f a b => (x -> a) -> f x -> b
liftAn f = apN . fmap f

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

test6 = liftAn (++) (Just "a") (Just "b")

test7 = (liftAn . liftAn) (++) (Just (Just "a")) Nothing
