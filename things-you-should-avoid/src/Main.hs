{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE Rank2Types        #-}

module Main where

import           Control.Monad
import           System.IO.Unsafe
import           Unsafe.Coerce

class Avoid a where
    avoid :: a -> void

data False
instance Avoid False where
    avoid = \case{}

data AnythingIsTrue = AnythingIsTrue (forall a. a)
instance Avoid AnythingIsTrue where
    avoid (AnythingIsTrue void) = void

newtype TypeLevelLoop = TypeLevelLoop TypeLevelLoop
instance Avoid TypeLevelLoop where
    avoid (TypeLevelLoop loop) = avoid loop

data TermLevelLoop = TermLevelLoop
instance Avoid TermLevelLoop where
    avoid = avoid

data Exception = Exception
instance Avoid Exception where
    avoid Exception = error "You should have avoided this"

data IncompleteImplementation = IncompleteImplementation
instance Avoid IncompleteImplementation

instance {-# OVERLAPPABLE #-} Avoid a where
    avoid = unsafeCoerce

data UnsafePerformIO = UnsafePerformIO
instance Avoid UnsafePerformIO where
    avoid UnsafePerformIO =
        unsafePerformIO . forever $ putStrLn "I will not use 'unsafePerformIO' anymore"

newtype FalsePromise a = FalsePromise (Avoid a => a)
instance Avoid (FalsePromise a) where
    avoid (FalsePromise x) = avoid x

newtype Success cost = Success cost
newtype AtAllCosts f = AtAllCosts
    { unAtAllCosts  :: forall cost. f cost
    }
instance Avoid (AtAllCosts Success) where
    avoid (AtAllCosts (Success void)) = void

main :: IO ()
main = mempty
