{-# LANGUAGE FlexibleInstances #-}

module Main where

import Lib
import Feeder
import Events
import Util
import Control.Monad.Free
import Text.Printf
import Text.Format

fileName :: String
fileName = "log"

instance InterpretingContext IO where
  
  run (Free (Commit next)) = do
    contents <- readFile fileName
    let commitDay = show $ (+1) $ getLastDayNumber (lines contents)
    putStrLn ("committing day " ++ commitDay)
    appendFile fileName (commitDay ++ "\n")
    run next
  
  run (Free (Rollback next)) = do
    contents <- readFile fileName
    let allLines = (lines contents)
        lastDay = case reverse allLines of
          [] -> "n/a"
          (x:_) -> x
        butLast = case allLines of
          [] -> []
          list -> init list
    putStrLn ("rolling back day " ++ lastDay)
    writeFile fileName ""
    traverse (\l -> appendFile fileName (l ++ "\n")) butLast
    run next

  run (Free (GetCurrent out)) = do
    contents <- readFile fileName
    let lastDay = getLastDayNumber (lines contents)
    putStrLn $ "current day is: " ++ (show lastDay)
    run $ out lastDay

  run (Free (RenderCalendar currentDay next)) = do
    _ <- putStrLn "day / new feed / old feed"
    let calendar = getCalendar
    _ <- traverse putStrLn (map (\tuple -> proportionFormatter (snd tuple) ((fst tuple) == currentDay) )  (zipWith (\i e -> (i, e)) [1..] calendar))
    run next

  run (Free Done) = return ()


testLogic :: Free Event ()
testLogic = do
  commitLogic
  rollbackLogic
  commitLogic
  getCalendarStateLogic
  done

getCalendarStateLogic :: Free Event ()
getCalendarStateLogic = do
  currentDay <- getCurrent
  renderCalendar currentDay
  done

commitLogic :: Free Event ()
commitLogic = do
  commit
  getCalendarStateLogic
  done

rollbackLogic :: Free Event ()
rollbackLogic = do
  rollback
  getCalendarStateLogic
  done

proportionFormatter :: Proportion -> Bool -> String
proportionFormatter (Proportion day newFeed oldFeed) isCurrent =
  format "{0}       {1}         {2}{3}" [(printf "%2d." day), show newFeed, show oldFeed, (if isCurrent then " <-- You are here" else "")]

main :: IO ()
main = do
  run testLogic :: IO ()

-- TODO
-- command line args to match the scenario logic
