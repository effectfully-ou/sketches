module Main where

import           Control.Exception
import           Data.IORef
import           System.IO.Unsafe

lastIsFalse :: [Bool]
lastIsFalse = unsafePerformIO $ do
    next <- newIORef False
    let go = do
                b <- unsafeInterleaveIO $ readIORef next
                bs <- unsafeInterleaveIO $ do
                    writeIORef next True
                    _ <- evaluate b
                    writeIORef next False
                    go
                pure $ b : bs
    go

foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' f = foldr (($!) . f)

-- >>> init . take 1 $ zip [1..] lastIsFalse
-- []
-- >>> last . take 1 $ zip [1..] lastIsFalse
-- (1,False)

-- >>> init . take 2 $ zip [1..] lastIsFalse
-- [(1,True)]
-- >>> last . take 2 $ zip [1..] lastIsFalse
-- (2,False)

-- >>> init . take 5 $ zip [1..] lastIsFalse
-- [(1,True),(2,True),(3,True),(4,True)]
-- >>> last . take 5 $ zip [1..] lastIsFalse
-- (5,False)

-- >>> take 5 $ zip [1..] lastIsFalse
-- [(1,False),(2,False),(3,False),(4,False),(5,False)]

-- >>> foldr' (:) [] . take 5 $ zip [1..] lastIsFalse
-- [(1,True),(2,True),(3,True),(4,True),(5,False)]

-- >>> traverse pure . take 5 $ zip [1..] lastIsFalse
-- [(1,True),(2,True),(3,True),(4,True),(5,False)]

-- >>> pure . take 5 $ zip [1..] lastIsFalse
-- [(1,False),(2,False),(3,False),(4,False),(5,False)]

-- >>> traverse pure . take 3 $ zip [1..] lastIsFalse
-- [(1,True),(2,True),(3,False)]
-- >>> traverse pure . take 5 $ zip [1..] lastIsFalse
-- [(1,True),(2,True),(3,False),(4,True),(5,False)]

main :: IO ()
main = mempty
