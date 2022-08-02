{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mineunifier.Game where

import Mineunifier.Core
import Mineunifier.IO

import Data.Kind
import GHC.TypeLits

class Game (number :: Nat) where
    type family ToSolve number :: [[Symbol]]
    type family ToCheck number :: [[Cell]] -> Constraint

class Check against puzzle
instance (Parse against answer, Verify answer puzzle) => Check against puzzle

instance Game 0 where
    type ToSolve 0 =
        '[ '[]
         ]
    type ToCheck 0 = Check
        '[ '[]
         ]

instance Game 1 where
    type ToSolve 1 =
        '[ '["?"]
         ]
    type ToCheck 1 = Check
        '[ '["0"]
         ]

instance Game 2 where
    type ToSolve 2 =
        '[ '["?"]
         ]
    type ToCheck 2 = Check
        '[ '["x"]
         ]

instance Game 3 where
    type ToSolve 3 =
        '[ '["?", "1"]
         ]
    type ToCheck 3 = Check
        '[ '["x", "1"]
         ]

instance Game 4 where
    type ToSolve 4 =
        '[ '["?", "x"]
         ]
    type ToCheck 4 = Check
        '[ '["x", "x"]
         ]

instance Game 5 where
    type ToSolve 5 =
        '[ '["?", "?"]
         ]
    type ToCheck 5 = Check
        '[ '["x", "x"]
         ]

instance Game 6 where
    type ToSolve 6 =
        '[ '["?", "?"]
         ]
    type ToCheck 6 = Check
        '[ '["x", "1"]
         ]

instance Game 7 where
    type ToSolve 7 =
        '[ '["?", "?"]
         , '["?", "?"]
         ]
    type ToCheck 7 = Check
        '[ '["x", "x"]
         , '["x", "x"]
         ]

instance Game 8 where
    type ToSolve 8 =
        '[ '["?", "?"]
         , '["?", "1"]
         ]
    type ToCheck 8 = Check
        '[ '["1", "1"]
         , '["x", "1"]
         ]

instance Game 9 where
    type ToSolve 9 =
        '[ '["?", "?"]
         , '["x", "1"]
         ]
    type ToCheck 9 = Check
        '[ '["1", "1"]
         , '["x", "1"]
         ]

instance Game 10 where
    type ToSolve 10 =
        '[ ["?", "3", "?", "?"]
         , ["2", "?", "?", "?"]
         , ["2", "3", "4", "?"]
         , ["?", "3", "3", "2"]
         , ["2", "?", "?", "?"]
         ]
    type ToCheck 10 = Check
        '[ ["x", "3", "3", "2"]
         , ["2", "x", "x", "x"]
         , ["2", "3", "4", "x"]
         , ["x", "3", "3", "2"]
         , ["2", "x", "x", "1"]
         ]

instance Game 11 where
    type ToSolve 11 =
        '[ '["?", "?", "?", "?", "?"]
         , '["?", "?", "4", "2", "1"]
         , '["?", "?", "3", "0", "0"]
         , '["?", "?", "2", "0", "0"]
         , '["?", "?", "1", "0", "0"]
         ]
    type ToCheck 11 = Check
        '[ '["1", "2", "x", "x", "1"]
         , '["2", "x", "4", "2", "1"]
         , '["3", "x", "3", "0", "0"]
         , '["2", "x", "2", "0", "0"]
         , '["1", "1", "1", "0", "0"]
         ]

instance Game 12 where
    type ToSolve 12 =
        '[ '["0", "0", "2", "?", "?"]
         , '["0", "0", "2", "?", "?"]
         , '["1", "1", "1", "?", "?"]
         , '["?", "?", "?", "?", "?"]
         , '["?", "?", "?", "?", "?"]
         ]
    type ToCheck 12 = Check
        '[ '["0", "0", "2", "x", "x"]
         , '["0", "0", "2", "x", "3"]
         , '["1", "1", "1", "1", "1"]
         , '["x", "2", "0", "0", "0"]
         , '["x", "2", "0", "0", "0"]
         ]

instance Game 13 where
    type ToSolve 13 =
        '[ '["0", "0", "0", "2", "?"]
         , '["0", "0", "0", "3", "?"]
         , '["1", "1", "0", "2", "?"]
         , '["?", "1", "0", "2", "?"]
         , '["?", "1", "0", "1", "?"]
         ]
    type ToCheck 13 = Check
        '[ '["0", "0", "0", "2", "x"]
         , '["0", "0", "0", "3", "x"]
         , '["1", "1", "0", "2", "x"]
         , '["x", "1", "0", "2", "2"]
         , '["1", "1", "0", "1", "x"]
         ]

instance Game 14 where
    type ToSolve 14 =
        '[ '["?", "1", "0", "0", "0"]
         , '["?", "2", "1", "0", "0"]
         , '["?", "?", "1", "0", "0"]
         , '["?", "?", "1", "1", "1"]
         , '["?", "?", "?", "?", "?"]
         ]
    type ToCheck 14 = Check
        '[ '["1", "1", "0", "0", "0"]
         , '["x", "2", "1", "0", "0"]
         , '["3", "x", "1", "0", "0"]
         , '["x", "3", "1", "1", "1"]
         , '["x", "2", "0", "1", "x"]
         ]

instance Game 15 where
    type ToSolve 15 =
        '[ '["?", "?", "?", "1", "0", "0", "0"]
         , '["2", "3", "2", "1", "0", "0", "0"]
         , '["0", "0", "0", "0", "0", "0", "0"]
         , '["1", "2", "1", "2", "1", "1", "0"]
         , '["?", "?", "?", "?", "?", "1", "0"]
         , '["?", "?", "?", "3", "1", "1", "0"]
         , '["?", "?", "?", "1", "0", "0", "0"]
         ]
    type ToCheck 15 = Check
        '[ '["x", "x", "x", "1", "0", "0", "0"]
         , '["2", "3", "2", "1", "0", "0", "0"]
         , '["0", "0", "0", "0", "0", "0", "0"]
         , '["1", "2", "1", "2", "1", "1", "0"]
         , '["x", "3", "x", "2", "x", "1", "0"]
         , '["x", "5", "2", "3", "1", "1", "0"]
         , '["x", "3", "x", "1", "0", "0", "0"]
         ]

instance Game 16 where
    type ToSolve 16 =
        '[ '["?", "?", "?", "?", "?", "?", "?"]
         , '["?", "?", "?", "?", "?", "?", "?"]
         , '["?", "?", "?", "?", "?", "?", "?"]
         , '["1", "1", "2", "?", "?", "?", "?"]
         , '["0", "0", "2", "?", "2", "1", "1"]
         , '["0", "0", "1", "1", "1", "0", "0"]
         , '["0", "0", "0", "0", "0", "0", "0"]
         ]
    type ToCheck 16 = Check
        '[ '["2", "x", "x", "2", "1", "1", "0"]
         , '["x", "4", "2", "3", "x", "2", "0"]
         , '["x", "2", "1", "3", "x", "3", "1"]
         , '["1", "1", "2", "x", "3", "2", "x"]
         , '["0", "0", "2", "x", "2", "1", "1"]
         , '["0", "0", "1", "1", "1", "0", "0"]
         , '["0", "0", "0", "0", "0", "0", "0"]
         ]

instance Game 17 where
    type ToSolve 17 =
        '[ '["?", "?", "?", "1", "0", "0", "0"]
         , '["?", "?", "?", "2", "0", "1", "1"]
         , '["?", "?", "?", "1", "0", "2", "?"]
         , '["?", "?", "?", "1", "0", "2", "?"]
         , '["?", "?", "?", "1", "1", "1", "?"]
         , '["?", "?", "?", "?", "?", "?", "?"]
         , '["?", "?", "?", "?", "?", "?", "?"]
         ]
    type ToCheck 17 = Check
        '[ '["0", "1", "x", "1", "0", "0", "0"]
         , '["1", "3", "3", "2", "0", "1", "1"]
         , '["1", "x", "x", "1", "0", "2", "x"]
         , '["1", "2", "2", "1", "0", "2", "x"]
         , '["1", "1", "1", "1", "1", "1", "1"]
         , '["x", "2", "2", "x", "1", "1", "1"]
         , '["2", "x", "2", "1", "1", "1", "x"]
         ]
