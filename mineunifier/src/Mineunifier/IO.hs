{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mineunifier.IO where

import Mineunifier.Core

import Data.Proxy
import GHC.TypeLits
import Data.Type.Equality

class Parse source result
instance result ~~ '[] => Parse '[] result
instance (rs ~~ (r ': rs'), Parse s r, Parse ss' rs') => Parse (s ': ss') rs
instance                        Parse "?" c
instance c ~~ 'X             => Parse "x" c
instance c ~~ 'N (FromNat 0) => Parse "0" c
instance c ~~ 'N (FromNat 1) => Parse "1" c
instance c ~~ 'N (FromNat 2) => Parse "2" c
instance c ~~ 'N (FromNat 3) => Parse "3" c
instance c ~~ 'N (FromNat 4) => Parse "4" c
instance c ~~ 'N (FromNat 5) => Parse "5" c
instance c ~~ 'N (FromNat 6) => Parse "6" c
instance c ~~ 'N (FromNat 7) => Parse "7" c
instance c ~~ 'N (FromNat 8) => Parse "8" c

class ShowField a where
    showField :: String

instance ShowField 'X where
    showField = "x"

instance KnownNat (ToNat m) => ShowField ('N m) where
    showField = show . natVal $ Proxy @(ToNat m)

instance ShowField '[] where
    showField = ""

instance (ShowField el, ShowField row) => ShowField (el : row :: [Cell]) where
    showField = showField @el ++ " " ++ showField @row

instance ShowField '[[]] where
    showField = ""

instance (ShowField row, ShowField rows) => ShowField (row : rows :: [[Cell]]) where
    showField = showField @row ++ "\n" ++ showField @rows
