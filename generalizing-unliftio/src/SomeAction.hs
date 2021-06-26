module SomeAction where

import           Control.Monad.IO.Class

printM :: (MonadIO m, Show a) => a -> m ()
printM = liftIO . print

class MonadIO m => SomeAction m where
    someAction :: m ()
    someAction = liftIO $ putStrLn "performed some action"
