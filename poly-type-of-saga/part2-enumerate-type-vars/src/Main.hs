{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Main where

import           Data.Bifunctor
import           Data.Functor
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Typeable
import           GHC.TypeLits

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

type Var :: forall k. Nat -> k
data family Var i

type EnumerateFromTo :: forall b. Nat -> Nat -> b -> Constraint
class EnumerateFromTo i j y | i y -> j
instance
    ( EnumerateFromTo i j f
    , EnumerateFromTo j k x
    ) => EnumerateFromTo i k (f x)
instance {-# INCOHERENT #-}
    ( x ~ Var @b i
    , x ~?~ y
    , j ~ If (x === y) (i + 1) i
    ) => EnumerateFromTo i j (y :: b)

data VarInfo = VarInfo
    { _varInfoId   :: Integer              -- ^ The id of the variable.
    , _varInfoKind :: ReifiedType VarInfo  -- ^ The kind of a type variable. Which is in fact a type.
                                           -- I.e. we pretend we have `TypeInType`.
    } deriving (Eq)

data ReifiedType var
    = RVar var                                    -- ^ A type variable.
    | RApply (ReifiedType var) (ReifiedType var)  -- ^ A type-level application.
    | RArrow                                      -- ^ @(->)@
    | RStar                                       -- ^ @*@
    | RMeta String                                -- ^ A meta type (like 'Int' or '[]').
    deriving (Eq, Foldable)

class ReifyType (a :: k) where
    reifyType :: Proxy a -> ReifiedType VarInfo

instance ReifyType (*) where
    reifyType _ = RStar

instance ReifyType (->) where
    reifyType _ = RArrow

instance {-# OVERLAPPING #-} (KnownNat i, ReifyType k) => ReifyType (Var i :: k) where
    reifyType _ = RVar $ VarInfo (natVal @i Proxy) (reifyType $ Proxy @k)

instance {-# OVERLAPS #-} (ReifyType f, ReifyType a) => ReifyType (f a) where
    reifyType _ = RApply (reifyType $ Proxy @f) (reifyType $ Proxy @a)

instance {-# OVERLAPPABLE #-} Typeable any => ReifyType any where
    reifyType = RMeta . show . typeRepTyCon . typeRep

infixl 4 :$
data PolyType
    = Var String
    | PolyType :$ PolyType
    | Arrow
    | Star
    | Meta String
    | Forall [(String, PolyType)] PolyType

extractVars :: ReifiedType VarInfo -> [VarInfo]
extractVars = nub . collectVars where
    collectVars = uncurry (++) . foldMap (\var -> (collectVars $ _varInfoKind var, [var]))

type VarNames = [String]

defaultVarNames :: (VarNames, VarNames, VarNames)
defaultVarNames = (hks, tys, els) where
    hks = ["f", "g", "h"]
    tys = ["a", "b", "c", "d"]
    els = ["x", "y", "z"]

pickVarName :: VarNames -> (String, VarNames)
pickVarName []             = error "not enough names"
pickVarName (name : names) = (name, names)

toVarIdNames :: [VarInfo] -> [(Integer, String)]
toVarIdNames = go defaultVarNames where
    go _ [] = []
    go (hks, tys, els) (VarInfo n kind : vars) = (n, a) : go names' vars where
        (a, names') = case kind of
            RStar                      -> pickVarName tys <&> \tys' -> (hks, tys', els)
            RApply (RApply RArrow _) _ -> pickVarName hks <&> \hks' -> (hks', tys, els)
            _                          -> pickVarName els <&> \els' -> (hks, tys, els')

toFinalTypeBody :: (Integer -> String) -> ReifiedType VarInfo -> PolyType
toFinalTypeBody toVarName = go where
    go (RVar (VarInfo varId _)) = Var $ toVarName varId
    go (RApply fun arg)         = go fun :$ go arg
    go RArrow                   = Arrow
    go RStar                    = Star
    go (RMeta name)             = Meta name

toPolyType :: ReifiedType VarInfo -> PolyType
toPolyType ty0 = addForalls $ toFinalTypeBody toVarName ty0 where
    vars0 = extractVars ty0
    varIdNames = toVarIdNames vars0
    toVarName varId = fromMaybe (error "internal error") $ lookup varId varIdNames

    toBinding (VarInfo varId kind) = (toVarName varId, toFinalTypeBody toVarName kind)
    addForalls body = Forall (map toBinding vars0) body

instance Show PolyType where
    showsPrec p e = case e of
       Var name -> showParen (p > 10) $ showString name
       Meta "[]" :$ x -> showParen (p > 10) $ showString "[" . showsPrec 1 x . showString "]"
       Meta "(,)" :$ x :$ y ->
           showParen (p > 10)
               $ showString "("
               . showsPrec 1 x
               . showString ", "
               . showsPrec 1 y
               . showString ")"
       Meta name -> showParen (p > 10) $ showString name
       Arrow :$ x :$ y -> showParen (p > 2) $ showsPrec 3 x . showString " -> " . showsPrec 2 y
       Arrow -> showParen (p > 10) $ showString "(->)"
       Star -> showParen (p > 10) $ showString "*"
       f :$ x -> showParen (p > 9) $ showsPrec 9 f . showString " " . showsPrec 10 x
       Forall []    body -> showParen (p > 10) $ showsPrec 1 body
       Forall binds body ->
           showParen (p > 10)
               $ showString "forall"
               . showString (binds >>= \(name, kind) -> " (" ++ name ++ " :: " ++ show kind ++ ")")
               . showString ". "
               . showsPrec 1 body

polyTypeOf :: forall a k. (ReifyType a, EnumerateFromTo 0 k a) => a -> PolyType
polyTypeOf _ = toPolyType . reifyType $ Proxy @a

instance Eq (Var i)
instance Num (Var i)
instance Foldable (Var i)
instance Bifunctor (Var i)

-- >>> import Data.Bifunctor
-- >>> import Data.Proxy
-- >>> polyTypeOf True
-- Bool
-- >>> polyTypeOf "I'm a string"
-- [Char]
-- >>> polyTypeOf $ \x y -> x
-- forall (a :: *) (b :: *). a -> b -> a
-- >>> polyTypeOf ($)
-- forall (a :: *) (b :: *). (a -> b) -> a -> b
-- >>> polyTypeOf $ let fix f = f (fix f) in fix
-- forall (a :: *). (a -> a) -> a
-- >>> polyTypeOf map
-- forall (a :: *) (b :: *). (a -> b) -> [a] -> [b]
-- >>> polyTypeOf 3
-- forall (a :: *). a
-- >>> polyTypeOf elem
-- forall (a :: *) (f :: * -> *). a -> f a -> Bool
-- >>> polyTypeOf bimap
-- forall (a :: *) (b :: *) (c :: *) (d :: *) (f :: * -> * -> *). (a -> b) -> (c -> d) -> f a c -> f b d
-- >>> polyTypeOf Proxy
-- <interactive>:608:2-17: error:
--     • No instance for (Typeable k0) arising from a use of ‘polyTypeOf’
--     • In the expression: polyTypeOf Proxy
--       In an equation for ‘it’: it = polyTypeOf Proxy

main :: IO ()
main = mempty
