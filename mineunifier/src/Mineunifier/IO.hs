{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
instance result ~~ '[] => Parse '[] result
instance (Parse s r, Parse ss' rs', rs ~~ (r ': rs')) => Parse (s ': ss') rs

class DisplayGamey a where
    displayGamey :: String

instance DisplayGamey 'X where
    displayGamey = "x"

instance KnownNat (ToNat m) => DisplayGamey ('N m) where
    displayGamey = show . natVal $ Proxy @(ToNat m)

instance DisplayGamey '[] where
    displayGamey = ""

instance (DisplayGamey el, DisplayGamey row) => DisplayGamey (el ': row :: [Cell]) where
    displayGamey = displayGamey @el ++ " " ++ displayGamey @row

instance (DisplayGamey row, DisplayGamey rows) => DisplayGamey (row ': rows :: [[Cell]]) where
    displayGamey = displayGamey @row ++ "\n" ++ displayGamey @rows

displayBoard :: forall input result. (Parse input result, DisplayGamey result) => String
displayBoard = displayGamey @result

-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> putStrLn $ displayBoard @('[ ["1", "1", "0"], ["x", "1", "0"] ])
-- 1 1 0
-- x 1 0
-- >>> putStrLn $ displayBoard @('[ ["1", "1", "0"], ["?", "1", "0"] ])
-- <interactive>:546:13: error:
--     • Ambiguous type variable ‘r0’ arising from a use of ‘displayBoard’
--       prevents the constraint ‘(DisplayGamey r0)’ from being solved.
--       Probable fix: use a type annotation to specify what ‘r0’ should be.
--       These potential instances exist:
--         instance [safe] KnownNat (ToNat m) => DisplayGamey ('N m)
--           -- Defined at /tmp/danteoGX7Hm.hs:41:10
--         instance [safe] DisplayGamey 'X
--           -- Defined at /tmp/danteoGX7Hm.hs:38:10
--     • In the second argument of ‘($)’, namely
--         ‘displayBoard @('[["1", "1", "0"], ["?", "1", "0"]])’
--       In the expression:
--         putStrLn $ displayBoard @('[["1", "1", "0"], ["?", "1", "0"]])
--       In an equation for ‘it’:
--           it = putStrLn $ displayBoard @('[["1", "1", "0"], ["?", "1", "0"]])
