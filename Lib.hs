module Lib where

import Control.Monad.IO.Class (liftIO, MonadIO)

-- | TODO: maybe should be log-level aware?
putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn


