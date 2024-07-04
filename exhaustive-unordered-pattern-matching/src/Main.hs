{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main (module Main) where

data R = R
    { a :: Int
    , b :: Int
    }

serializeA :: Int -> IO ()
serializeA = mempty

serializeB :: Int -> IO ()
serializeB = mempty

dup :: a -> (a, a)
dup x = (x, x)

-- infixr 0 :&
-- pattern (:&) :: a -> a -> a
-- pattern p1 :& p2 <- (dup -> (p1, p2))
-- {-# COMPLETE (:&) #-}

pattern (:&) :: a -> a -> a
pattern a0:&a1 <- a0@a1
{-# COMPLETE (:&) #-}

serializeR :: R -> IO ()
serializeR (R _ _ :& R {b, a}) = do
    serializeA a
    serializeB b

main :: IO ()
main = pure ()
