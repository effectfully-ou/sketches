{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.IORef
import Data.Function
import Data.Foldable
import Data.Profunctor
import Control.Monad
import Control.Exception
import MonadVar as Var

forceFoldable :: Foldable f => f a -> f a
forceFoldable xs = foldl' (flip seq) () xs `seq` xs

type Optic i o s t a b = i a b -> o s t

type Lens s t a b = forall p. Strong p => Optic p p s t a b

class Readable i a | i -> a where
  readable :: Optic i o s t a b -> o s t

class Writeable i where
  (.~) :: Optic i o s t a b -> b -> o s t

class Mutateable i where
  (%~) :: Optic i o s t a b -> (a -> b) -> o s t

class MutateableF i f | i -> f where
  (^~) :: Optic i o s t a b -> (a -> f b) -> o s t

instance Readable (Forget a) a where
  readable _Lens = _Lens $ Forget id

instance Writeable (->) where
  _Lens .~ y = _Lens $ const y

instance Mutateable (->) where
  _Lens %~ f = _Lens f

newtype Action action a b = Action (action a b)

data ReadAction r a b where
  ReadAction :: ReadAction a a b

data AlterAction a b
  = WriteAction b
  | MutateAction (a -> b)

newtype MutateFAction f a b = MutateFAction (a -> f b)

deriving instance Profunctor action => Profunctor (Action action)
deriving instance Strong     action => Strong     (Action action)

instance Profunctor AlterAction where
  dimap _ g (WriteAction  y) = WriteAction  $ g y
  dimap f h (MutateAction g) = MutateAction $ h . g . f

instance Strong AlterAction where
  first'  (WriteAction  y) = MutateAction $ \(_, z) -> (y, z)
  first'  (MutateAction f) = MutateAction $ \(x, z) -> (f x, z)

  second' (WriteAction  y) = MutateAction $ \(z, _) -> (z, y)
  second' (MutateAction f) = MutateAction $ \(z, x) -> (z, f x)

instance Functor f => Profunctor (MutateFAction f) where
  dimap f h (MutateFAction g) = MutateFAction $ fmap h . g . f

instance Functor f => Strong (MutateFAction f) where
  first'  (MutateFAction f) = MutateFAction $ \(x, z) -> (, z) <$> f x
  second' (MutateFAction f) = MutateFAction $ \(z, x) -> (z ,) <$> f x

instance action ~ ReadAction a => Readable (Action action) a where
  readable _Lens = _Lens $ Action ReadAction

instance (action ~ AlterAction) => Writeable (Action action) where
  _Lens .~ y = _Lens $ Action (WriteAction y)

instance (action ~ AlterAction) => Mutateable (Action action) where
  _Lens %~ f = _Lens $ Action (MutateAction f)

instance (action ~ MutateFAction f) => MutateableF (Action action) f where
  _Lens ^~ f = _Lens $ Action (MutateFAction f)

class Variable action m v r | action -> r where
  _Var :: Optic (Action action) (->) (v a) (m r) a a

instance MonadRead m v => Variable (ReadAction a) m v a where
  _Var (Action ReadAction) var = Var.read var

instance MonadMutate_ m v => Variable AlterAction m v () where
  _Var (Action (WriteAction  y)) var = Var.write   var y
  _Var (Action (MutateAction f)) var = Var.mutate_ var f

instance MonadMutateM_ f m v => Variable (MutateFAction f) m v () where
  _Var (Action (MutateFAction f)) var = Var.mutateM_ var f

newtype WeakAction action a b = WeakAction (action a b)

newtype WeakWriteAction a b = WeakWriteAction b

newtype ModifyAction a b = ModifyAction (a -> b)

deriving instance Profunctor action => Profunctor (WeakAction action)

instance Profunctor WeakWriteAction where
  dimap _ g (WeakWriteAction y) = WeakWriteAction $ g y

deriving instance Profunctor ModifyAction

instance action ~ ReadAction a => Readable (WeakAction action) a where
  readable _Lens = _Lens $ WeakAction ReadAction

instance (action ~ WeakWriteAction) => Writeable (WeakAction action) where
  _Lens .~ y = _Lens $ WeakAction (WeakWriteAction y)

instance (action ~ ModifyAction) => Mutateable (WeakAction action) where
  _Lens %~ f = _Lens $ WeakAction (ModifyAction f)

instance (action ~ MutateFAction f) => MutateableF (WeakAction action) f where
  _Lens ^~ f = _Lens $ WeakAction (MutateFAction f)

class File action r | action -> r where
  _File :: Optic (WeakAction action) (->) String (IO r) String String

instance File (ReadAction a) a where
  _File (WeakAction ReadAction) file = readFile file

instance File WeakWriteAction () where
  _File (WeakAction (WeakWriteAction s)) file = writeFile file s

instance File ModifyAction () where
  _File (WeakAction (ModifyAction f)) file =
    readFile file >>= evaluate . forceFoldable . f >>= writeFile file

infixr 9 ./, !/, .!
infixr 4 .~, %~, ^~
infixl 8 ^., ^!

_1 :: Lens (a, c) (b, c) a b
_1 = first'

_2 :: Lens (a, b) (a, c) b c
_2 = second'

_Of :: (s -> a) -> Lens s b a b
_Of = lmap

(^.) :: s -> Optic (Forget a) (Forget a) s t a b -> a
a ^. _Lens = runForget (readable _Lens) a

(^!) :: Readable i a => s -> Optic i (->) s t a b -> t
(^!) = flip readable

(./)
  :: Strong o
  => Optic (Forget s) (Forget s) v x s y
  -> Optic  i          o         s t a b
  -> Optic  i          o         v t a b
_G ./ _U = _Of (^. _G) . _U

(!/)
  :: (Readable i s, Monad m)
  => Optic i (->) u (m v) s y
  -> Optic j (->) v (m t) a b
  -> Optic j (->) u (m t) a b
_G !/ _U = \u -> readable _G >=> _U u

newtype ProFunctor p f a b = ProFunctor
  { getProFunctor :: p a (f b)
  }

instance (Profunctor p, Functor f) => Profunctor (ProFunctor p f) where
  dimap g f (ProFunctor a) = ProFunctor $ dimap g (fmap f) a

instance (Strong p, Functor f) => Strong (ProFunctor p f) where
  first'  (ProFunctor a) = ProFunctor . rmap (\(fx, y) -> (, y) <$> fx) $ first'  a
  second' (ProFunctor a) = ProFunctor . rmap (\(x, gy) -> (x ,) <$> gy) $ second' a

effectful :: Functor f => Lens s t a b -> Lens s (f t) a (f b)
effectful _L = getProFunctor . _L . ProFunctor

(.!)
  :: (Strong o, Functor f)
  => Lens      v  w    s t
  -> Optic i o s (f t) a b
  -> Optic i o v (f w) a b
_L .! _M = effectful _L . _M

main :: IO ()
main = do
  "stuff.txt" & _File .~ "stuff"
  "stuff.txt" ^! _File >>= putStrLn

  -- Write "stuff_2.txt" to `stuff.txt`.
  "stuff.txt" & _File .~ "stuff_2.txt"
  -- Write "text" to `stuff_2.txt`.
  "stuff_2.txt" & _File .~ "text"
  fileVar <- newIORef "stuff.txt"
  -- Replicate each letter twice in a file which name is written in a file
  -- which name is stored in the `fileVar` variable.
  fileVar & _Var !/ _File !/ _File %~ concatMap (replicate 2)
  -- Read and print the result.
  fileVar ^! _Var !/ _File !/ _File >>= putStrLn

  -- Setting and modifying work alright for the pure case.
  print $ ('a', 'b') & _1 .~ 'c'          -- ('c', 'b')
  print $ ('a', 'b') & _2 %~ succ         -- ('a', 'c')

  -- Create a new variable.
  var <- newIORef 'a'
  -- Set the variable (uses only `writeIORef` internally).
  var & _Var .~ 'b'
  -- Read the variable (uses only `readIORef` internally) and print the result.
  var ^! _Var >>= print                   -- 'b' -- what is printed.
  -- Mutate the variable.
  var & _Var %~ succ
  -- Read the variable and print the result.
  var ^! _Var >>= print                   -- 'c'
  -- Monadically modify the variable.
  var & _Var ^~ \c -> succ c <$ print c   -- 'c'
  -- Read the variable and print the result.
  var ^! _Var >>= print                   -- 'd'
  -- Mutate the variable stored inside a structure (discards the structure).
  ('d', var) & _2 ./ _Var %~ succ
  -- Read the variable and print the result.
  var ^! _Var >>= print                   -- 'e'

  -- Create a new variable
  var2 <- newIORef ('a', 'c')  
  -- Mutate the variable stored inside a structure (discards the structure).
  (('d', var2), 'a') & _1 ./ _2 ./ _Var . _1 %~ succ
  -- Read the variable stored inside a structure and print the result.
  (('d', var2), 'a') ^! _1 ./ _2 ./ _Var >>= print -- ('b', 'c')
  -- Mutate the variable stored inside a structure (discards the structure).
  (('d', var2), 'a') & (_1 . _2) ./ _Var . _1 %~ succ
  -- Read the variable and print the result.
  var2 ^! _Var >>= print                           -- ('c', 'c')
