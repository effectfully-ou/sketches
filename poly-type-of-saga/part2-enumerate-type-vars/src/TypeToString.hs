{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TypeToString where

import           Data.Proxy

class TypeToString a where
    typeToString :: Proxy a -> String

instance TypeToString Bool where
    typeToString _ = "Bool"

instance TypeToString Int where
    typeToString _ = "Int"

instance (TypeToString a, TypeToString b) => TypeToString (a, b) where
    typeToString _ = concat ["(", typeToString (Proxy @a), ", ", typeToString (Proxy @b), ")"]

instance TypeToString a => TypeToString [a] where
    typeToString _ = concat ["[", typeToString (Proxy @a), "]"]

-- >>> :set -XTypeApplications
-- >>> import Data.Proxy
-- >>> typeToString $ Proxy @[(Int, Bool)]
-- "[(Int, Bool)]"
-- >>> import Data.Typeable
-- >>> typeRep $ Proxy @[(Int, Bool)]
-- [(Int,Bool)]
