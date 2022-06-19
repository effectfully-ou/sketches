{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mineunifier.Util where

import GHC.TypeLits

type family HeadDef z xs where
    HeadDef z '[]      = z
    HeadDef _ (x ': _) = x

type family Take n xs where
    Take 0 _         = '[]
    Take _ '[]       = '[]
    Take n (x ': xs) = x ': Take (n - 1) xs

type family Drop n xs where
    Drop 0 xs        = xs
    Drop _ '[]       = '[]
    Drop n (x ': xs) = Drop (n - 1) xs

infixr 5 ++
type family xs ++ ys where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

type family Concat xss where
    Concat '[]         = '[]
    Concat (xs ': xss) = xs ++ Concat xss
