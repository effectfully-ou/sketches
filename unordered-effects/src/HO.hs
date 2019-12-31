{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
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

module HO where

import           Control.Lens
import qualified Control.Monad.Except as Mtl
import           Control.Monad.Morph  hiding (embed)
import qualified Control.Monad.Reader as Mtl
import qualified Control.Monad.State  as Mtl
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Data.Proxy
import           Data.Sum
import           GHC.Exts

class f y x => Flip (f :: b -> a -> Constraint) x y
instance f y x => Flip (f :: b -> a -> Constraint) x y

type family All (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    All _ '[]       = ()
    All f (x ': xs) = (f x, All f xs)

type Effect = (* -> *) -> * -> *

class Call h (eff :: Effect) where
    _Call :: Prism' (h m a) (eff m a)

class All (Call h) effs => Sig h effs
instance All (Call h) effs => Sig h effs

type Lifter effs m
    =  forall effs' b
    .  effs' `In` effs
    => Proxy effs'
    -> (forall h. Sig h effs' => h (EffT effs' m) b)
    -> m b

newtype WLifter effs m = WLifter
    { unWLifter :: Lifter effs m
    }

newtype EffT effs m a = EffT
    { unEff :: Lifter effs m -> m a
    } deriving
        ( Functor, Applicative, Monad
        , Mtl.MonadError e, Mtl.MonadState s
        ) via ReaderT (WLifter effs m) m

instance MonadTrans (EffT effs) where
    lift a = EffT $ \_ -> a

instance Mtl.MonadReader r m => Mtl.MonadReader r (EffT effs m) where
    ask = lift Mtl.ask
    local f (EffT k) = EffT $ \h -> Mtl.local f $ k h

class (forall f. Sig f effs => Call f eff) => eff `Member` effs
instance (forall f. Sig f effs => Call f eff) => eff `Member` effs

-- 'In' is reflexive.
send :: forall eff effs m a. eff `Member` effs => eff (EffT effs m) a -> EffT effs m a
send a = EffT $ \h -> h (Proxy @effs) $ _Call # a

class (forall f. Sig f effs2 => Sig f effs1) => effs1 `In` effs2
instance (forall f. Sig f effs2 => Sig f effs1) => effs1 `In` effs2

-- 'In' is transitive.
embed :: effs1 `In` effs2 => EffT effs1 m a -> EffT effs2 m a
embed (EffT k) = EffT k

-- inMember :: '[eff] `In` effs => Proxy ('(,) eff effs) -> (eff `Member` effs => c) -> c
-- inMember _ = id

memberIn :: eff `Member` effs => Proxy ('(,) eff effs) -> ('[eff] `In` effs => c) -> c
memberIn _ = id

class All (Flip Member effs2) effs1 => effs1 `Members` effs2
instance All (Flip Member effs2) effs1 => effs1 `Members` effs2

membersIn2 :: '[eff1, eff2] `Members` effs => Proxy ('(,,) eff1 eff2 effs) -> ('[eff1, eff2] `In` effs => c) -> c
membersIn2 _ = id

-- membersIn :: effs1 `Members` effs1 => Proxy ('(,) effs1 effs2) -> (effs1 `In` effs2 => c) -> c
-- membersIn _ = id



newtype HO eff m a = HO
    { unHO :: eff a
    }

data Local r m a = Local
    { _localFun :: r -> r
    , _localArg :: m a
    }

ho :: Iso' (f a) (HO f m a)
ho = iso HO unHO



data TestF m b
    = TestReader (Reader Bool   b)
    | TestLocal  (Local  Bool m b)
    | TestState  (State  String b)
    | TestEither (Either Char   b)

instance Call TestF (HO (Reader Bool)) where
    _Call = prism' TestReader (\case
        TestReader b -> Just b
        _            -> Nothing) . ho

instance Call TestF (Local Bool) where
    _Call = prism' TestLocal (\case
        TestLocal b -> Just b
        _           -> Nothing)

instance Call TestF (HO (State String)) where
    _Call = prism' TestState (\case
        TestState b -> Just b
        _           -> Nothing) . ho

instance Call TestF (HO (Either Char)) where
    _Call = prism' TestEither (\case
        TestEither b -> Just b
        _            -> Nothing) . ho

runExample
    :: m ~ ReaderT Bool (StateT String (Either Char))
    => EffT '[HO (Either Char), HO (Reader Bool), Local Bool, HO (State String)] m a
    -> m a
runExample (EffT k) = k $ const $ \case
    TestReader         b  -> hoist generalize b
    TestLocal (Local f b) -> local f . runExample $ embed b
    TestState          b  -> lift $ hoist generalize b
    TestEither         b  -> lift $ lift b



a1 :: Monad m => EffT '[HO (Either Char), HO (Reader Bool), Local Bool] m Int
a1 = send $ Local not $ do
    b <- send @(HO (Reader Bool)) $ HO ask
    if b
        then return 0
        else send $ HO $ Left 'a'

a2 :: '[HO (Either Char), HO (Reader Bool)] `Members` effs => EffT effs m Int
a2 = send @(HO (Either Char)) $ HO $ Right 1

a3 :: Monad m => EffT '[HO (State String), HO (Reader Bool)] m Int
a3 = do
    send @(HO (State String)) $ HO $ modify (++ "text")
    length <$> send @(HO (State String)) (HO get)

a123
    :: ( Monad m
       , '[HO (Either Char), HO (Reader Bool), Local Bool, HO (State String)] `Members` effs
       )
    => EffT effs m Int
a123 = do
    x1 <- embed a1
    x2 <- a2
    x3 <- embed a3
    return $ x1 + x2 + x3

-- Left 'a'
test1 :: Either Char Int
test1 = evalStateT (runReaderT (runExample a123) True) ""

-- Right 5
test2 :: Either Char Int
test2 = evalStateT (runReaderT (runExample a123) False) ""




class InterpretIn m f where
    interpret :: f a -> m a

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

class HInterpretIn m h where
    hinterpret :: InterpretIn m f => h f a -> m a

instance InterpretIn m f => HInterpretIn m (HO f) where
    hinterpret = interpret . unHO

instance Mtl.MonadReader r m => HInterpretIn m (Local r) where
    hinterpret (Local fun arg) = Mtl.local fun $ interpret arg

instance (All (HInterpretIn m) effs, m ~ m') => InterpretIn m' (EffT effs m) where
    interpret = undefined

runEffT :: All (HInterpretIn m) effs => EffT effs m a -> m a
runEffT = interpret
