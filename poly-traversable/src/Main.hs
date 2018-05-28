{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, DefaultSignatures, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, ScopedTypeVariables, TypeOperators, TypeApplications #-}
module Main where

import           GHC.Exts (Constraint)
import           Data.Word
import           Data.Proxy
import           Data.Monoid
import           Data.Foldable (toList)
import           Data.Functor.Identity
import           Data.Functor.Const
import           Data.Functor.Compose
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text

type family Element c
type instance Element (f a) = a
type instance Element IntSet = Int
type instance Element ByteString = Word8
type instance Element Text = Char

data PolyShape (f :: * -> *)
data MonoShape s

type family ShapeOf s where
  ShapeOf (f _) = PolyShape f
  ShapeOf  s    = MonoShape s

type family SpecifyShapeBy s t where
  SpecifyShapeBy (PolyShape f) t = f (Element t)
  SpecifyShapeBy (MonoShape s) _ = s

type s `DeterminesShapeOf` t = t ~ SpecifyShapeBy (ShapeOf s) t
type s `SameShape` t = (s `DeterminesShapeOf` t, t `DeterminesShapeOf` s)

class ShapeOf s ~ ss => KnownShapeDispatch s ss where
  withKnownShape :: proxy s -> (forall f a. s ~ f a => c) -> (ShapeOf s ~ MonoShape s => c) -> c

instance s ~ f a => KnownShapeDispatch s (PolyShape f) where
  withKnownShape _ poly _ = poly

instance (ShapeOf s ~ MonoShape s, s ~ s') => KnownShapeDispatch s (MonoShape s') where
  withKnownShape _ _ mono = mono

type KnownShape s = KnownShapeDispatch s (ShapeOf s)

withReflexiveShapeOf :: forall s c. KnownShape s => s -> (s `SameShape` s => c) -> c
withReflexiveShapeOf _ x = withKnownShape (Proxy :: Proxy s) x x

withSymmetricShapes :: s `SameShape` t => Proxy (s, t) -> (t `SameShape` s => c) -> c
withSymmetricShapes _ x = x

withTransitiveShapes
  :: forall s t u c. (KnownShape s, s `SameShape` t, t `SameShape` u)
  => Proxy (s, t, u) -> (s `SameShape` u => c) -> c
withTransitiveShapes _ x = withKnownShape (Proxy :: Proxy s) x x

-- Just the same as `MonoFoldable`.
class PolyFoldable s where
  pfoldMap :: Monoid m => (Element s -> m) -> s -> m
  default pfoldMap :: (Monoid m, s ~ f a, Foldable f) => (Element s -> m) -> s -> m
  pfoldMap = foldMap

class s `SameShape` s => PolyFunctor s where
  pmap :: s `SameShape` t => (Element s -> Element t) -> s -> t
  default pmap :: (s ~ f a, t ~ f b, Functor f) => (Element s -> Element t) -> s -> t
  pmap = fmap

class (PolyFunctor s, PolyFoldable s) => PolyTraversable s where
  ptraverse :: (s `SameShape` t, Applicative g) => (Element s -> g (Element t)) -> s -> g t
  default ptraverse :: (Applicative g, s ~ f a, t ~ f b, Traversable f) => (Element s -> g (Element t)) -> s -> g t
  ptraverse = traverse

omap :: PolyFunctor s => (Element s -> Element s) -> s -> s
omap = pmap

pfoldMapDefault :: forall s m. (PolyTraversable s, Monoid m) => (Element s -> m) -> s -> m
pfoldMapDefault f = getConst . ptraverse @s @s (Const . f)

pfoldMapViaFoldr
  :: Monoid m
  => (forall b. (Element s -> b -> b) -> b -> s -> b)
  -> (Element s -> m) -> s -> m
pfoldMapViaFoldr fr f = fr (mappend . f) mempty

-- Could be `ptraverseViaIso` which receives an `Iso s t (f (Element s)) (f (Element t))`.
ptraverseViaPackUnpack
  :: (Applicative g, Traversable f)
  => (f (Element t) -> t)
  -> (s -> f (Element s))
  -> (Element s -> g (Element t)) -> s -> g t
ptraverseViaPackUnpack pack unpack f = fmap pack . traverse f . unpack

instance PolyFunctor [a]
instance PolyFunctor (Maybe a)
instance PolyFunctor (Const b a)

instance PolyFoldable [a]
instance PolyFoldable (Maybe a)
instance PolyFoldable (Const b a)

instance PolyTraversable [a]
instance PolyTraversable (Maybe a)
instance PolyTraversable (Const b a)

instance PolyFunctor IntSet where
  pmap = IntSet.map
instance PolyFunctor ByteString where
  pmap = BS.map
instance PolyFunctor Text where
  pmap = Text.map

instance PolyFoldable IntSet where
  pfoldMap = pfoldMapViaFoldr IntSet.foldr
instance PolyFoldable ByteString where
  pfoldMap = pfoldMapViaFoldr BS.foldr
instance PolyFoldable Text where
  pfoldMap = pfoldMapViaFoldr Text.foldr

instance PolyTraversable IntSet where
  ptraverse = ptraverseViaPackUnpack IntSet.fromList IntSet.toList
instance PolyTraversable ByteString where
  ptraverse = ptraverseViaPackUnpack BS.pack BS.unpack
instance PolyTraversable Text where
  ptraverse = ptraverseViaPackUnpack Text.pack Text.unpack

main :: IO ()
main = mempty
