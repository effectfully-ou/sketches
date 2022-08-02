{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mineunifier.Core where

import Mineunifier.Util

import Data.Kind
import GHC.TypeLits

data Peano = Z | S Peano

type family FromNat (n :: Nat) :: Peano where
    FromNat 0 = 'Z
    FromNat n = 'S (FromNat (n - 1))

type family ToNat (m :: Peano) :: Nat where
    ToNat 'Z     = 0
    ToNat ('S m) = ToNat m + 1

--------------------

data Cell
    = X
    | N Peano

--------------------

class Rule (n :: a) (c :: Cell) (p :: a)

instance {-# INCOHERENT #-} n ~ 'S p => Rule n 'X     p
instance {-# INCOHERENT #-} n ~ p    => Rule n ('N m) p

instance {-# INCOHERENT #-} (n' ~ 'Z, c ~ 'X)  => Rule ('S n') c 'Z
instance {-# INCOHERENT #-} (c ~ 'N m, p ~ 'Z) => Rule 'Z      c p

instance {-# INCOHERENT #-} (p ~ 'S p', Rule ('S n') c p') => Rule ('S ('S n')) c p
instance {-# INCOHERENT #-} (n ~ 'S n', Rule n'      c p') => Rule n            c ('S p')

--------------------

class NeighbsToRulesGo (n :: a) (nb :: Cell) (nbs :: [Cell])
instance Rule n nb 'Z => NeighbsToRulesGo n nb '[]
instance (Rule n nb p, NeighbsToRulesGo p nb' nbs) => NeighbsToRulesGo n nb (nb' ': nbs)

type family NeighbsToRules (c :: Cell) (nbs :: [Cell]) :: Constraint where
    NeighbsToRules 'X     _           = ()
    NeighbsToRules ('N n) '[]         = n ~ 'Z
    NeighbsToRules ('N n) (nb ': nbs) = NeighbsToRulesGo n nb nbs

--------------------

type family MakeRulesRow ss ps cs ns :: Constraint where
    MakeRulesRow _  _  '[] _        = ()
    MakeRulesRow ss ps (c ': cs) ns =
        ( NeighbsToRules c (Take 3 ss ++ Take 2 ps ++ Take 1 cs ++ Take 2 ns)
        , MakeRulesRow (Take 1 ps ++ '[c] ++ Take 1 ns) (Drop 1 ps) cs (Drop 1 ns)
        )

type family MakeRulesGo ps (css :: [[Cell]]) :: Constraint where
    MakeRulesGo _ '[]          = ()
    MakeRulesGo ps (cs ': css) =
        ( MakeRulesRow '[] ps cs (HeadDef '[] css)
        , MakeRulesGo cs css
        )

type MakeRules result = MakeRulesGo '[] result

--------------------

class Reveal (answer :: a) (puzzle :: a)
instance answer ~ 'X   => Reveal answer 'X
instance answer ~ 'N p => Reveal answer ('N p)
instance answer ~ '[]  => Reveal answer '[]
instance (as ~ (a ': as'), Reveal a p, Reveal as' ps') => Reveal as (p ': ps')

type family CountXs (a :: k) :: Nat where
    CountXs 'X        = 1
    CountXs (y ': ys) = CountXs y + CountXs ys
    CountXs _         = 0

class Verify (answer :: [[Cell]]) (puzzle :: [[Cell]])
instance
    ( Reveal answer puzzle
    , NeighbsToRules ('N (FromNat (CountXs answer))) (Concat puzzle)
    ) => Verify answer puzzle
