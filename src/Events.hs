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

class InterpretingContext a where
  run :: Free Event () -> a
