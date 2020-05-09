{-# LANGUAGE GADTSyntax #-}

module HFM where

import           Control.Monad.Free
import           System.Random

data LogLevel = Info
type Message  = String

-- Algebra (interface) for the LoggerL Free monadic language with only 1 method
data LoggerF next where
  LogMessage :: LogLevel -> Message -> (() -> next) -> LoggerF next

-- Functor instance needed for the Free machinery
instance Functor LoggerF where
  fmap f (LogMessage lvl msg next) = LogMessage lvl msg (f . next)

-- Free monadic language
type Logger a = Free LoggerF a

data AppF next where
  GetRandomInt :: (Int, Int) -> (Int -> next) -> AppF next
  EvalLogger :: Logger () -> (() -> next) -> AppF next

instance Functor AppF where
  fmap f (GetRandomInt range next) = GetRandomInt range (f . next)
  fmap f (EvalLogger logAct next)  = EvalLogger logAct (f . next)

type App a = Free AppF a

-- Simple console logger
interpretLoggerF :: LoggerF a -> IO a
interpretLoggerF (LogMessage lvl msg next) = do
  putStrLn msg
  pure $ next ()

runLogger :: Logger a -> IO a
runLogger = foldFree interpretLoggerF

-- Interpreting function
interpretAppF :: AppF a -> IO a
interpretAppF (EvalLogger loggerAct next) = next <$> runLogger loggerAct
interpretAppF (GetRandomInt range next)   = next <$> randomRIO range

-- Interpreter entry point
runApp :: App a -> IO a
runApp = foldFree interpretAppF

-- Log message with Info level.
logInfo :: Message -> App ()
logInfo msg = evalLogger (logMessage Info msg)

-- Helper function to wrap LoggerF method
logMessage :: LogLevel -> Message -> Logger ()
logMessage lvl msg = liftF $ LogMessage lvl msg id

-- Helper function to wrap AppF method
evalLogger :: Logger () -> App ()
evalLogger logger = liftF $ EvalLogger logger id

getRandomInt :: (Int, Int) -> App Int
getRandomInt range = liftF $ GetRandomInt range id

printRandomFactorial :: App ()
printRandomFactorial = do
    n <- getRandomInt (1, 100)
    logInfo $ show $ product [1..n]
