{-# LANGUAGE MultiParamTypeClasses, DefaultSignatures, FunctionalDependencies, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module FunDep where

import           Data.Word
import           Data.Foldable
import           Data.Functor.Const
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as Text

class PolyFunctor s t a b | s -> a, t -> b, s b -> t, t a -> s where
  pmap :: (a -> b) -> s -> t
  default pmap :: (Functor f, s ~ f a, t ~ f b) => (a -> b) -> s -> t
  pmap = fmap

class PolyFoldable s a | s -> a where
  ptoList :: s -> [a]
  default ptoList :: (Foldable f, s ~ f a) => s -> [a]
  ptoList = toList

class (PolyFunctor s t a b, PolyFoldable s a) => PolyTraversable s t a b | s -> a, t -> b, s b -> t, t a -> s where
  ptraverse :: Applicative g => (a -> g b) -> s -> g t
  default ptraverse :: (Traversable f, Applicative g, s ~ f a, t ~ f b) => (a -> g b) -> s -> g t
  ptraverse = traverse

pfoldMapDefault :: forall s a m. (PolyTraversable s s a a, Monoid m) => (a -> m) -> s -> m
pfoldMapDefault f = getConst . ptraverse @s @s (Const . f)

instance PolyFunctor ByteString Text Word8 Char where
  pmap f = Text.pack . map f . BS.unpack
