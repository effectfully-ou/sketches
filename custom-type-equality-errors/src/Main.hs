{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitForAll           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Main where

import           Data.Kind
import           Data.Type.Map
import           GHC.TypeLits

type CheckEqualKV :: forall k v. k -> k -> v -> v -> Constraint
type family CheckEqualKV k1 k2 v1 v2 where
    CheckEqualKV k1 k1 v1 v1 = ()
    CheckEqualKV k1 k1 v1 v2 =
        TypeError
            ( 'ShowType v1 ':<>:
              'Text " is not equal to " ':<>:
              'ShowType v2 ':<>:
              'Text " at " ':<>:
              'ShowType k1
            )
    CheckEqualKV k1 k2 v1 v2 =
        TypeError
            ( 'ShowType (k1 ':-> v1) ':<>:
              'Text " is not equal to " ':<>:
              'ShowType (k2 ':-> v2)
            )

type EqualKV :: forall k v. k -> k -> v -> v -> Constraint
class (k1 ~ k2, v1 ~ v2) => EqualKV k1 k2 v1 v2
instance (CheckEqualKV k1 k2 v1 v2, k1 ~ k2, v1 ~ v2) => EqualKV k1 k2 v1 v2

-- Ext :: Var k -> v -> Map m -> Map ((k :-> v) ': m)
ext :: EqualKV k1 k2 v1 v2 => Var k1 -> v1 -> Map m -> Map ((k2 ':-> v2) ': m)
ext = Ext

-- error:
--     • "w" ':-> Bool is not equal to "w" ':-> Int
--     • In the expression: ext (Var :: (Var "w")) False
--       <...>
foo :: Map '["x" ':-> Int, "z" ':-> Bool, "w" ':-> Int]
foo = ext (Var :: (Var "x")) 2
    $ ext (Var :: (Var "z")) True
    $ ext (Var :: (Var "w")) False
    $ Empty

-- >>> :t bar
-- bar :: Map '[ "x" ':-> Int, "z" ':-> Bool, "w" ':-> Int]
bar = ext (Var :: (Var "x")) (2 :: Int)
    $ ext (Var :: (Var "z")) True
    $ ext (Var :: (Var "w")) (5 :: Int)
    $ Empty

main :: IO ()
main = mempty
