module Util where

import Control.Applicative (Alternative)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum, Foldable)
import Q (Q)

-- | for-alternative: rather than performing actions using applicative
-- sequence, perform them using alternative <|>
-- (c.f. for in Traversable (?))
forA_ :: (Foldable t, Functor t, Alternative a) => t x -> (x -> a y) -> a y
forA_ l f = asum (fmap f l)

report :: String -> Q any ()
report s = liftIO $ putStrLn s

