{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.List
import           NoThunks.Class
import           System.Environment

fibs_zw :: [Integer]
fibs_zw = 0 : 1 : zipWith (+) fibs_zw (tail fibs_zw)

fib_zw :: Int -> Integer
fib_zw n = fibs_zw !! n

infix 4 `cons'`
cons' :: a -> [a] -> [a]
cons' !x xs = x : xs

at' :: Int -> [a] -> a
at' n0 | n0 < 0 = error "bad index"
at' n0 = go n0 where
    go _ []        = error "not enough elements"
    go 0 (x:_)     = x
    go n ((!_):xs) = go (n - 1) xs

fib_zw1' :: Int -> Integer
fib_zw1' n = at' n fibs_zw

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = go where
    go (x : xs) (y : ys) = f x y `cons'` go xs ys
    go _        _        = []

fibs_zw' :: [Integer]
fibs_zw' = 0 : 1 : zipWith' (+) fibs_zw (tail fibs_zw)

fib_zw2' :: Int -> Integer
fib_zw2' n = fibs_zw' !! n

fibs_sl' :: [Integer]
fibs_sl' = 0 : scanl' (+) 1 fibs_sl'

fib_sl' :: Int -> Integer
fib_sl' n = fibs_sl' !! n

forceElems :: [a] -> [a]
forceElems = foldr cons' []

fib_zwfe :: Int -> Integer
fib_zwfe n = forceElems fibs_zw !! n

fib_loop :: Int -> Integer
fib_loop n0 = go n0 0 1 where
    go 0 curr _    = curr
    go n curr next = go (n - 1) next $! curr + next

throwOnThunks :: NoThunks a => a -> a
throwOnThunks x = case unsafeNoThunks x of
    Nothing -> x
    Just _  -> error "A thunk!"

thunkElemsToErrorsLazy :: NoThunks a => [a] -> [a]
thunkElemsToErrorsLazy = map throwOnThunks

thunkElemsToErrors :: NoThunks a => [a] -> [a]
thunkElemsToErrors = forceElems . thunkElemsToErrorsLazy

unsafeFib_zw :: Int -> Integer
unsafeFib_zw n = thunkElemsToErrors fibs_zw !! n

unsafeFib_zw1' :: Int -> Integer
unsafeFib_zw1' n = thunkElemsToErrors fibs_zw' !! n

unsafeFib_zwfe :: Int -> Integer
unsafeFib_zwfe n = thunkElemsToErrors (forceElems fibs_zw) !! n

main :: IO ()
main = do
    [mode] <- getArgs
    let run = print . length . show
    case mode of
        "zw" -> run $ fib_zw 500000
        "zw1s" -> run $ fib_zw1' 500000
        "zw2s" -> run $ fib_zw2' 500000
        "sls" -> run $ fib_sl' 500000
        "zwfe" -> run $ fib_zwfe 500000
        "zw_nothunks" -> run $ unsafeFib_zw 500000
        "zw1s_nothunks" -> run $ unsafeFib_zw1' 500000
        "zwfe_nothunks" -> run $ unsafeFib_zwfe 500000
        "zw2s_2" -> do
            run $ fib_zw2' 100000
            run $ fib_zw2' 99999
        "loop" -> do
            run $ fib_loop 100000
            run $ fib_loop 99999
        _ -> fail "wrong mode"
