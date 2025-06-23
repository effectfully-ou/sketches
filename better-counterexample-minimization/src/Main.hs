{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveFoldable      #-}

module Main (module Main) where

import           Test.QuickCheck

data Tree a
    = Nil
    | Branch a (Tree a) (Tree a)
    deriving (Show)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = undefined

-- >>> import Test.QuickCheck
-- >>> mapM_ print . shrink $ Branch True (Branch True Nil Nil) (Branch True Nil Nil)
-- Nil
-- Branch True Nil Nil
-- Branch True Nil Nil
-- Branch False (Branch True Nil Nil) (Branch True Nil Nil)
-- Branch True Nil (Branch True Nil Nil)
-- Branch True Nil (Branch True Nil Nil)
-- Branch True Nil (Branch True Nil Nil)
-- Branch True (Branch False Nil Nil) (Branch True Nil Nil)
-- Branch True (Branch True Nil Nil) Nil
-- Branch True (Branch True Nil Nil) Nil
-- Branch True (Branch True Nil Nil) Nil
-- Branch True (Branch True Nil Nil) (Branch False Nil Nil)
    -- shrink :: Tree a -> [Tree a]
    -- shrink Nil = []
    -- shrink (Branch x l r) =
    --   -- shrink Branch to Nil
    --   [Nil] ++
    --   -- shrink to subterms
    --   [l, r] ++
    --   -- recursively shrink subterms
    --   [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]

-- >>> import Test.QuickCheck
-- >>> mapM_ print . shrink $ Branch True (Branch True Nil Nil) (Branch True Nil Nil)
-- Nil
-- Branch True Nil Nil
-- Branch True Nil Nil
-- Branch False (Branch True Nil Nil) (Branch True Nil Nil)
-- Branch True Nil (Branch True Nil Nil)
-- Branch True (Branch False Nil Nil) (Branch True Nil Nil)
-- Branch True (Branch True Nil Nil) Nil
-- Branch True (Branch True Nil Nil) (Branch False Nil Nil)
    -- shrink :: Tree a -> [Tree a]
    -- shrink Nil = []
    -- shrink (Branch x l r) =
    --   -- shrink Branch to Nil
    --   [Nil] ++
    --   -- shrink to subterms
    --   [t | t@Branch{} <- [l, r]] ++
    --   -- recursively shrink subterms
    --   [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]

-- >>> import Test.QuickCheck
-- >>> mapM_ print . shrink $ Branch True (Branch True Nil Nil) (Branch True Nil Nil)
-- Nil
-- Branch True Nil Nil
-- Branch True Nil Nil
-- Branch True Nil (Branch True Nil Nil)
-- Branch True (Branch True Nil Nil) Nil
-- Branch False (Branch True Nil Nil) (Branch True Nil Nil)
-- Branch True (Branch False Nil Nil) (Branch True Nil Nil)
-- Branch True (Branch True Nil Nil) (Branch False Nil Nil)
    shrink :: Tree a -> [Tree a]
    shrink = uncurry (++) . go where
        go :: Tree a -> ([Tree a], [Tree a])
        go Nil = ([], [])
        go (Branch x l r) = (tSp, tEl) where
            (lSp, lEl) = go l
            (rSp, rEl) = go r
            tSp = concat
                [ [Nil]
                , [t | t@Branch{} <- [l, r]]
                , [Branch x l' r | l' <- lSp]
                , [Branch x l r' | r' <- rSp]
                ]
            tEl = concat
                [ [Branch x' l r | x' <- shrink x]
                , [Branch x l' r | l' <- lEl]
                , [Branch x l r' | r' <- rEl]
                ]

-- >>> import Test.QuickCheck
-- >>> mapM_ print . shrink $ Branch (Branch True Nil Nil) Nil (Branch (Branch True Nil Nil) Nil Nil)
-- Nil
-- Branch (Branch True Nil Nil) Nil Nil
-- Branch (Branch True Nil Nil) Nil Nil
-- Branch Nil Nil (Branch (Branch True Nil Nil) Nil Nil)
-- Branch (Branch False Nil Nil) Nil (Branch (Branch True Nil Nil) Nil Nil)
-- Branch (Branch True Nil Nil) Nil (Branch Nil Nil Nil)
-- Branch (Branch True Nil Nil) Nil (Branch (Branch False Nil Nil) Nil Nil)

-- class PriorityShrink a where
--     priorityShrink :: a -> ([a], [a])

-- shrinkViaPriorityShrink :: PriorityShrink a => a -> [a]
-- shrinkViaPriorityShrink = uncurry (++) . priorityShrink

-- instance PriorityShrink a => PriorityShrink (Tree a) where
--     priorityShrink = undefined

-- instance PriorityShrink a => Arbitrary (Tree a) where
--     arbitrary = undefined
--     shrink = shrinkViaPriorityShrink

main :: IO ()
main = pure ()
