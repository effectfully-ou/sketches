{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.Environment
import Except
import qualified Leak
import qualified Fine

multifail :: (Except f [Int], Applicative f) => f [Int]
multifail = go 1000000 where
  go 0 = pure []
  go n = throw [n] *> go (n - 1)

main :: IO ()
main = do
  args <- getArgs
  print . sum $ case args of
    ["leak"] -> let Leak.Failure ns = multifail in ns
    ["fine"] -> let Fine.Failure ns = multifail in ns
    _        -> fail "don't play with me"
