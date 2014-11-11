module Util where

import Control.Applicative (Alternative, pure)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (asum, Foldable)
import Q (Q)

-- | for-alternative: rather than performing actions using applicative
-- sequence, perform them using alternative <|>
-- (c.f. for in Traversable (?))
forA_ :: (Foldable t, Functor t, Alternative a) => t x -> (x -> a y) -> a y
forA_ l f = asum (fmap f l)

-- | Given a Traversable (eg a list) of values for type v,
-- return each one as an alternative result.
shred :: (Functor t, Foldable t, Alternative a) => t v -> a v
shred l = forA_ l pure

report :: String -> Q any ()
report s = liftIO $ putStrLn s

debugReport :: String -> Q any ()
debugReport _ = return ()
-- debugReport s = liftIO $ putStrLn s
