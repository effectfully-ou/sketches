{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Applicative
import Data.Proxy

data D
  = C0
  | C1 Int Bool
  | C2 Word
  | C3 Char

parseC0 :: Parse D
parseC1 :: Parse D
parseC3 :: Parse D

parseD :: Parse D
parseD = parseC0 <|> parseC1 <|> parseC3

class Warning warning where
  warning :: warning -> ()

data FIX_ME_BUT_FIRST request a
  = LOOK_RIGHT_ABOVE
  | FIX_ME_BUT_FIRST request a

data IMPLEMENT_PARSING_FOR = IMPLEMENT_PARSING_FOR

instance Warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR D) where
  warning LOOK_RIGHT_ABOVE                              = ()
  warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR C0{}) = ()
  warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR C1{}) = ()
  warning (FIX_ME_BUT_FIRST IMPLEMENT_PARSING_FOR C3{}) = ()

type Parse = Proxy
parseC0 = Proxy
parseC1 = Proxy
parseC3 = Proxy

main :: IO ()
main = mempty
