{-# LANGUAGE DeriveFunctor #-}

module Events
  ( Event (..)
  , commit
  , rollback
  , done
  , getCurrent
  , renderCalendar
  , InterpretingContext (..)
  ) where

import Control.Monad.Free
import Control.Monad
import Feeder

data Event next =
  Commit next |
  Rollback next |
  -- GetCurrent does not take input, but outputs an Int
  GetCurrent (Int -> next) |
  -- RenderCalendar takes an Int as an input argument
  RenderCalendar Int next |
  Done
  deriving (Functor)

commit :: Free Event ()
commit = liftF $ Commit ()

rollback :: Free Event ()
rollback = liftF $ Rollback ()

getCurrent :: Free Event Int
getCurrent = liftF (GetCurrent id)

renderCalendar :: Int -> Free Event ()
renderCalendar currentDay = liftF $ RenderCalendar currentDay ()

done :: Free Event ()
done = liftF Done

class Monad m => InterpretingContext m where
  run :: Free Event () -> m ()
