module FT where

import           System.Random

data LogLevel = Info
type Message  = String

class Monad m => MonadLogger m where
    logMessage :: LogLevel -> Message -> m ()

class MonadLogger m => MonadApp m where
    getRandomInt :: (Int, Int) -> m Int

instance MonadLogger IO where
    logMessage _ = putStrLn

instance MonadApp IO where
    getRandomInt = randomRIO

runMonadApp :: IO a -> IO a
runMonadApp = id

-- Corresponds to the original @logInfo :: Message -> App ()@
logInfo :: MonadApp m => Message -> m ()
logInfo = logMessage Info

printRandomFactorial :: MonadApp m => m ()
printRandomFactorial = do
    n <- getRandomInt (1, 100)
    logInfo $ show $ product [1..n]
