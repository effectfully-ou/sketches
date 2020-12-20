{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Main where

import           Data.Kind
import           Data.Type.Equality
import           Data.Typeable
import           Test.QuickCheck

infix 4 ===
type (===) :: forall a b. a -> b -> Bool
type family x === y where
    x === x = 'True
    x === y = 'False

type TryUnify :: forall a b. Bool -> a -> b -> Constraint
class same ~ (x === y) => TryUnify same x y
instance (x === y) ~ 'False => TryUnify 'False x y
instance {-# INCOHERENT #-} (x ~~ y, same ~ 'True) => TryUnify same x y

type (~?~) :: forall a b. a -> b -> Constraint
type x ~?~ y = TryUnify (x === y) x y

specifyAsInt :: Int -> Int
specifyAsInt = id

defaultToInt :: a ~?~ Int => a -> a
defaultToInt = id

defaultToDouble :: a ~?~ Double => a -> a
defaultToDouble = id

type DefaultAllTo :: forall a b. a -> b -> Constraint
class DefaultAllTo d y
instance (DefaultAllTo d f, DefaultAllTo d x) => DefaultAllTo d (f x)
instance {-# INCOHERENT #-} (DefaultAllTo d b, d ~?~ y) => DefaultAllTo d (y :: b)

defaultAllToInt :: DefaultAllTo Int a => a -> a
defaultAllToInt = id

defaultAllToList :: DefaultAllTo [] a => a -> a
defaultAllToList = id

defaultAllToTrue :: DefaultAllTo 'True a => a -> a
defaultAllToTrue = id

prop_reverseReverse :: Eq a => [a] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

-- >>> import Data.Proxy
-- >>> import Data.Typeable
-- >>> typeOf 'a'
-- Char
-- >>> typeOf (&&)
-- Bool -> Bool -> Bool
-- >>> typeOf id
-- <interactive>:176:2-10: error:
--     • No instance for (Typeable a0) arising from a use of ‘typeOf’
--     • In the expression: typeOf id
--       In an equation for ‘it’: it = typeOf id
-- >>> :t 42
-- 42 :: Num p => p
-- >>> :t specifyAsInt 42
-- specifyAsInt 42 :: Int
-- >>> :t specifyAsInt True
-- <interactive>:1:14-17: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘Bool’
--     • In the first argument of ‘specifyAsInt’, namely ‘True’
--       In the expression: specifyAsInt True
-- >>> :t defaultToInt 42
-- defaultToInt 42 :: Int
-- >>> :t defaultToInt True
-- defaultToInt True :: Bool
-- >>> :t defaultToInt [42]
-- defaultToInt [42] :: Num a => [a]
-- >>> :t defaultAllToInt reverse
-- defaultAllToInt reverse :: [Int] -> [Int]
-- >>> :t defaultAllToInt elem
-- defaultAllToInt elem :: Foldable t => Int -> t Int -> Bool
-- >>> :set -fprint-explicit-foralls
-- >>> :t defaultAllToInt Proxy
-- defaultAllToInt Proxy :: forall {t :: Int}. Proxy t
-- >>> :unset -fprint-explicit-foralls
-- >>> :t defaultAllToList fmap
-- defaultAllToList fmap :: (a -> b) -> [a] -> [b]
-- >>> :t defaultAllToTrue Proxy
-- defaultAllToTrue Proxy :: Proxy 'True
-- >>> :t defaultAllToList $ defaultAllToInt fmap
-- defaultAllToList $ defaultAllToInt fmap
--   :: (Int -> Int) -> [Int] -> [Int]
-- >>> :t [defaultToInt 1, defaultToDouble 2]
-- [defaultToInt 1, defaultToDouble 2] :: [Int]
-- >>> :t [defaultToDouble 1, defaultToInt 2]
-- [defaultToDouble 1, defaultToInt 2] :: [Double]

quickCheckPoly :: (Testable prop, DefaultAllTo Int prop) => prop -> IO ()
quickCheckPoly = quickCheck

main :: IO ()
main = quickCheckPoly prop_reverseReverse
