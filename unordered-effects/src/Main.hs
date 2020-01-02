{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Main where

import           Control.Lens
import           Control.Monad
import qualified Control.Monad.Except as Mtl
import           Control.Monad.Morph  hiding (embed)
import qualified Control.Monad.Reader as Mtl
import qualified Control.Monad.State  as Mtl
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader hiding (local)
import           Data.Foldable
import           Data.Proxy
import           Data.Sum
import           GHC.Exts

-- Utils

class f y x => Flip (f :: b -> a -> Constraint) x y
instance f y x => Flip (f :: b -> a -> Constraint) x y

type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    All _ '[]       = ()
    All f (x ': xs) = (f x, All f xs)

-- Core

type Effect = * -> *

class Call f (eff :: Effect) where
    _Call :: Prism' (f a) (eff a)

class (Functor f, All (Call f) effs) => Sig f effs
instance (Functor f, All (Call f) effs) => Sig f effs

type Lifter effs m = forall b. (forall f. Sig f effs => Proxy f -> f b) -> m b

newtype WLifter effs m = WLifter
    { unWLifter :: Lifter effs m
    }

newtype EffT effs m a = EffT
    { unEffT :: Lifter effs m -> m a
    } deriving
        ( Functor, Applicative, Monad
        , Mtl.MonadError e, Mtl.MonadState s
        ) via ReaderT (WLifter effs m) m

instance MonadTrans (EffT effs) where
    lift a = EffT $ \_ -> a

instance Mtl.MonadReader r m => Mtl.MonadReader r (EffT effs m) where
    ask = lift Mtl.ask
    local f (EffT k) = EffT $ \b -> Mtl.local f $ k b

class (forall f. Sig f effs => Call f eff) => eff `Member` effs
instance (forall f. Sig f effs => Call f eff) => eff `Member` effs

send :: eff `Member` effs => eff a -> EffT effs m a
send a = EffT $ \h -> h $ \_ -> _Call # a

class (forall f. Sig f effs2 => Sig f effs1) => effs1 `In` effs2
instance (forall f. Sig f effs2 => Sig f effs1) => effs1 `In` effs2

embed :: effs1 `In` effs2 => EffT effs1 m a -> EffT effs2 m a
embed (EffT k) = EffT k

-- Entailments

-- -- Doesn't work.
-- inMember :: '[eff] `In` effs => Proxy ('(,) eff effs) -> (eff `Member` effs => c) -> c
-- inMember _ = id

memberIn :: eff `Member` effs => Proxy ('(,) eff effs) -> ('[eff] `In` effs => c) -> c
memberIn _ = id

class All (Flip Member effs2) effs1 => effs1 `Members` effs2
instance All (Flip Member effs2) effs1 => effs1 `Members` effs2

membersMember2
    :: '[eff1, eff2] `Members` effs
    => Proxy ('(,,) eff1 eff2 effs) -> ((eff1 `Member` effs, eff2 `Member` effs) => c) -> c
membersMember2 _ = id

memberMembers2
    :: (eff1 `Member` effs, eff2 `Member` effs)
    => Proxy ('(,,) eff1 eff2 effs) -> ('[eff1, eff2] `Members` effs => c) -> c
memberMembers2 _ = id

membersIn2
    :: '[eff1, eff2] `Members` effs
    => Proxy ('(,,) eff1 eff2 effs) -> ('[eff1, eff2] `In` effs => c) -> c
membersIn2 _ = id

-- -- Doesn't work.
-- membersIn :: effs1 `Members` effs1 => Proxy ('(,) effs1 effs2) -> (effs1 `In` effs2 => c) -> c
-- membersIn _ = id

-- Improved type inference

type family UnifyArgs (args :: k) :: Constraint where
    UnifyArgs '()                    = ()
    UnifyArgs ('(,) ('(,) x y) args) = (x ~ y, UnifyArgs args)

type family Unify (args :: m) (x :: k) (y :: l) :: Constraint where
    Unify args (f x) (g y) = Unify ('(,) ('(,) x y) args) f g
    Unify args x     x     = UnifyArgs args
    Unify args _     _     = ()

type family Find (x :: k) (xs :: [k]) :: Constraint where
    Find x '[]       = ()
    Find x (y ': xs) = (Unify '() x y, Find x xs)

sendFind :: (Find eff effs, eff `Member` effs) => eff a -> EffT effs m a
sendFind = send

-- `local`

mapEff :: eff `Member` effs => (forall a. eff a -> eff a) -> EffT effs m a -> EffT effs m a
mapEff f (EffT k) = EffT $ \h -> k $ \b -> h $ \p -> b p & _Call %~ f

local :: Reader r `Member` effs => (r -> r) -> EffT effs m a -> EffT effs m a
local f = mapEff @(Reader _) $ Mtl.local f

-- Hard

interpose
    :: forall eff effs m a. (Monad m, eff `Member` effs)
    => (forall a. eff a -> EffT effs m a) -> EffT effs m a -> EffT effs m a
interpose f (EffT k) =
    EffT $ \h -> k $ \b ->
        join $ h $ \p ->
            let bp = b p in
                case bp ^? _Call @_ @eff of
                    Nothing  -> pure <$> bp
                    Just eff -> unEffT (f eff) h <$ bp

-- General handling.

class InterpretIn m eff where
    interpret :: eff a -> m a

instance Mtl.MonadReader r m => InterpretIn m (Reader r) where
    interpret a = runReader a <$> Mtl.ask

instance Mtl.MonadError e m => InterpretIn m (Either e) where
    interpret = Mtl.liftEither

instance Mtl.MonadState s m => InterpretIn m (State s) where
    interpret a = do
        s <- Mtl.get
        let (x, s') = runState a s
        Mtl.put s'
        return x

instance eff :< effs => Call (Sum effs) eff where
    _Call = prism' inject project

runEffT
    :: forall effs m a. (Apply (InterpretIn m) effs, Sig (Sum effs) effs)
    => EffT effs m a -> m a
runEffT (EffT k) = k $ \b -> apply @(InterpretIn m) @effs interpret $ b Proxy

instance (Apply (InterpretIn m) effs, Sig (Sum effs) effs, m ~ m') =>
            InterpretIn m' (EffT effs m) where
    interpret = runEffT

-- Example.

a1 :: Monad m => EffT '[Either Char, Reader Bool] m Int
a1 = local not $ do
    b <- sendFind ask
    if b
        then return 0
        else send $ Left 'a'

a2 :: EffT '[Reader Bool, Either Char] m Int
a2 = sendFind $ Right 1

a3 :: Monad m => EffT '[State String, Reader Bool] m Int
a3 = do
    sendFind $ modify (++ "text")
    length <$> sendFind get

a123 :: Monad m => EffT '[Either Char, Reader Bool, State String] m Int
a123 = do
    x1 <- embed a1
    x2 <- embed a2
    x3 <- embed a3
    return $ x1 + x2 + x3

-- Ad hoc handling.

data TestF b
    = TestReader (Reader Bool   b)
    | TestState  (State  String b)
    | TestEither (Either Char   b)
    deriving (Functor)

instance Call TestF (Reader Bool) where
    _Call = prism' TestReader $ \case
        TestReader b -> Just b
        _            -> Nothing

instance Call TestF (State String) where
    _Call = prism' TestState $ \case
        TestState b -> Just b
        _           -> Nothing

instance Call TestF (Either Char) where
    _Call = prism' TestEither $ \case
        TestEither b -> Just b
        _            -> Nothing

runExample
    :: m ~ ReaderT Bool (StateT String (Either Char))
    => EffT '[Either Char, Reader Bool, State String] m Int
    -> m Int
runExample (EffT k) = k $ \b -> case b Proxy of
    TestReader b -> hoist generalize b
    TestState  b -> lift $ hoist generalize b
    TestEither b -> lift $ lift b

-- Left 'a'
test1' :: Either Char Int
test1' = evalStateT (runReaderT (runExample a123) True) ""

-- Right 5
test2' :: Either Char Int
test2' = evalStateT (runReaderT (runExample a123) False) ""

-- Left 'a'
test1 :: Either Char Int
test1 = evalStateT (runReaderT (runEffT a123) True) ""

-- Right 5
test2 :: Either Char Int
test2 = runReaderT (evalStateT (runEffT a123) "") False

main :: IO ()
main = traverse_ print [test1', test2', test1, test2]
