{-# LANGUAGE DeriveFunctor #-}

module Events
  ( Event (..)
  , commit
  , rollback
  , done
  , getCurrent
  , InterpretingContext (..)
  ) where

import Control.Monad.Free
import Control.Monad

data Event next =
  Commit next |
  Rollback next |
  GetCurrent next |
  Done
  deriving (Functor)

commit :: Free Event ()
commit = liftF $ Commit ()

rollback :: Free Event ()
rollback = liftF $ Rollback ()

getCurrent :: Free Event ()
getCurrent = liftF $ GetCurrent ()

done :: Free Event ()
done = liftF Done

class Monad m => InterpretingContext m where
  run :: Free Event () -> m ()
