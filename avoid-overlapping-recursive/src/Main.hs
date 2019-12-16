{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Main where

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

instance (Applicative f, DispatchApN f a b (IsFun b)) => ApN f a b where
    apN = dispatchApN (proxy# :: Proxy# (IsFun b))

liftAn :: ApN f a b => (x -> a) -> f x -> b
liftAn f = apN . fmap f

test1 :: (Enum a, Num a) => [a]
test1 = liftAn succ [1..5]

test2 :: (Enum a, Num a) => [a]
test2 = liftAn (+) [1..5] [3..5]

test3 :: (Enum a, Num a) => [a]
test3 = liftAn (\x y z -> x + y * z) [1..5] [3..5] [5]

test4 :: (Enum a, Num a) => Maybe (Maybe a)
test4 = (liftAn . liftAn) (+) (Just (Just 4)) (Just (Just 1))

test5 :: IO Int
test5 = do
    n <- liftAn (*) readLn readLn
    return $ n + 1

main :: IO ()
main = mempty
