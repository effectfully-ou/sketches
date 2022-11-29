{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Mineunifier.Test where

import Mineunifier.Core
import Mineunifier.Game
import Mineunifier.IO

play
    :: forall number (result :: [[Cell]]).
       (Parse (ToSolve number) result, MakeRules result, ToCheck number result, DisplayGamey result)
    => IO ()
play = do
    putStrLn "Solution:"
    putStrLn $ displayGamey @result

-- Solution:
-- 1 1 0 0 0
-- x 2 1 0 0
-- 3 x 1 0 0
-- x 3 1 1 1
-- x 2 0 1 x
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> play @0
-- Solution:
-- >>> play @1
-- Solution:
-- 0
-- >>> play @2
-- Solution:
-- x
-- >>> play @3
-- Solution:
-- x 1
-- >>> play @4
-- Solution:
-- x x
-- >>> play @5
-- Solution:
-- x x
-- >>> play @6
-- <interactive>:561:2: error:
--     • Could not deduce: (NeighbsToRules r0 '[r1],
--                          NeighbsToRules r1 '[r0])
--         arising from a use of ‘play’
--     • In the expression: play @6
--       In an equation for ‘it’: it = play @6
-- <interactive>:561:2: error:
--     • No instance for (Rule n'0 r1 'Z) arising from a use of ‘play’
--     • In the expression: play @6
--       In an equation for ‘it’: it = play @6
-- >>> play @7
-- Solution:
-- x x
-- x x
-- >>> play @8
-- <interactive>:563:2: error:
--     • Could not deduce: (NeighbsToRules r0 '[r1, r2, 'N ('S 'Z)],
--                          NeighbsToRules r1 '[r0, r2, 'N ('S 'Z)],
--                          NeighbsToRules r2 '[r0, r1, 'N ('S 'Z)])
--         arising from a use of ‘play’
--     • In the expression: play @8
--       In an equation for ‘it’: it = play @8
-- <interactive>:563:2: error:
--     • No instance for (Rule n'2 r1 n'1) arising from a use of ‘play’
--     • In the expression: play @8
--       In an equation for ‘it’: it = play @8
-- >>> play @9
-- Solution:
-- 1 1
-- x 1
-- >>> play @10
-- Solution:
-- 2 3 2
-- x x x
-- x x 3
-- >>> play @11
-- Solution:
-- x 3 3 2
-- 2 x x x
-- 2 3 4 x
-- x 3 3 2
-- 2 x x 1
-- >>> play @12
-- Solution:
-- 1 2 x x 1
-- 2 x 4 2 1
-- 3 x 3 0 0
-- 2 x 2 0 0
-- 1 1 1 0 0
-- >>> play @13
-- Solution:
-- 0 0 2 x x
-- 0 0 2 x 3
-- 1 1 1 1 1
-- x 2 0 0 0
-- x 2 0 0 0
-- >>> play @14
-- Solution:
-- 0 0 0 2 x
-- 0 0 0 3 x
-- 1 1 0 2 x
-- x 1 0 2 2
-- 1 1 0 1 x
-- >>> play @15
-- Solution:
-- 1 1 0 0 0
-- x 2 1 0 0
-- 3 x 1 0 0
-- x 3 1 1 1
-- x 2 0 1 x
-- >>> play @16
-- Solution:
-- x x x 1 0 0 0
-- 2 3 2 1 0 0 0
-- 0 0 0 0 0 0 0
-- 1 2 1 2 1 1 0
-- x 3 x 2 x 1 0
-- x 5 2 3 1 1 0
-- x 3 x 1 0 0 0
-- >>> play @17
-- Solution:
-- 2 x x 2 1 1 0
-- x 4 2 3 x 2 0
-- x 2 1 3 x 3 1
-- 1 1 2 x 3 2 x
-- 0 0 2 x 2 1 1
-- 0 0 1 1 1 0 0
-- 0 0 0 0 0 0 0
-- >>> play @18
-- Solution:
-- 0 1 x 1 0 0 0
-- 1 3 3 2 0 1 1
-- 1 x x 1 0 2 x
-- 1 2 2 1 0 2 x
-- 1 1 1 1 1 1 1
-- x 2 2 x 1 1 1
-- 2 x 2 1 1 1 x
