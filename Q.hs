{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Q where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.MonadPlus.Operational

import Data.Dynamic
import Data.Maybe
import Data.Typeable

import Lib

-- * The monad on which everything runs

class (Show q, Show (Answer q), Eq q, Eq (Answer q), Typeable q, Typeable (Answer q))
  => Qable q where

  type Answer q
  -- | Run the query. The answer type does not appear in
  -- the type signature because there is nothing magic
  -- about the answer type: running the query might push
  -- many different answers to many different queries in
  -- addition to this query.
  runQable :: Typeable final => q -> Q final ()

-- | QInstruction are the simple instructions to be used
-- inside the operational monad.

type QProgram final x = ProgramP (QInstruction final) x
newtype Q final x = Q (QProgram final x)
  deriving (Applicative, Alternative, Functor, Monad,
            MonadPlus)

instance MonadIO (Q final) where
  liftIO a = Q $ singleton $ QT a

data QInstruction final x where
  -- runs an action in the underlying IO monad
  QT :: IO r -> QInstruction final r
  -- | Asks for a query to be made without waiting for any answers
  QLaunch :: (Typeable final, Qable q) => q -> QInstruction final ()
  -- | Records an answer to a query (which might not have been launched...)
  QRecord :: (Typeable final, Qable q) => q -> (Answer q) -> QInstruction final ()
  -- | QPull asks for the responses to a query without launching that
  --   query. Often it should follow a push, I think, but sometimes not -
  --   for example when looking for cached values.
  -- but what should the type of this look like?
  -- TODO: I do not entirely remember why I need to 
  -- force typeable to be final here. It is used in PreviousPull
  -- somehow. Investigate if it is necessary (because it pushes
  -- that typeable constraint further out into client programs
  -- sometimes in a way that seems superficially unnecessary)

  QPull :: (Typeable final, Qable q) => q -> QInstruction final (Answer q)
  QPushFinalResult :: final -> QInstruction final ()

-- * DB bits

data PreviousLaunch where
  PreviousLaunch :: forall q . (Qable q) => q -> PreviousLaunch

deriving instance Show PreviousLaunch

instance Eq PreviousLaunch where
  (PreviousLaunch l) == (PreviousLaunch r) =
    Just l == cast r

data PreviousResult where
  PreviousResult :: forall q . (Qable q) => q -> (Answer q) -> ResultId -> PreviousResult

type ResultId = Integer

instance Show PreviousResult where 
  show (PreviousResult q a i) = "Query " ++ (show q) ++ " => " ++ (show a) ++ " [RESULT ID " ++ (show i) ++"]"

-- | a previous pull request. We can restart but only a computation that
-- eventually ends with no useful value returned. Potentially we could be
-- returning a value and discarding it, but using () makes it clearer
-- in the type system that there cannot be a value.
data PreviousPull final where
  PreviousPull :: forall final . forall q . (Typeable final, Qable q) => q -> PPQ final (Answer q) -> PreviousPull final

-- | this is a wrapper that gives a Typeable instance for a -> Q ()
-- because using that type on its own wasn't Typeable
data PPQ final a = PPQ (a -> Q final ()) deriving Typeable

data DB final = DB {
    previousLaunches :: [PreviousLaunch],
    previousResults :: [PreviousResult],
    previousPulls :: [PreviousPull final],
    finalResults :: [final]
  }

emptyDB = DB {
    previousLaunches = [],
    previousResults = [],
    previousPulls = [],
    finalResults = []
  }

qlaunch :: (Typeable final, Qable q) => q -> Q final (Answer q)
qlaunch q = Q $ (singleton $ QLaunch q) *> empty

qrecord q a = Q $ singleton $ QRecord q a

qpull q = Q $ singleton $ QPull q

query q = qlaunch q <|> qpull q

pushFinalResult v = Q $ singleton $ QPushFinalResult v
