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
    let commitDay = getCommitDayNumber (lines contents)
    putStrLn ("committing day " ++ show commitDay)
    appendFile fileName ((show commitDay) ++ "\n")
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

  run (Free (GetCurrent next)) = do
    contents <- readFile fileName
    let allLines = (lines contents)
        lastDay = case reverse allLines of
          [] -> "n/a"
          (x:_) -> x
    putStrLn $ "current day is: " ++ lastDay
    run next

  run (Free Done) = return ()


logic :: Free Event ()
logic = do
  getCurrent
  x <- commit
  rollback
  commit
  done

proportionFormatter :: Proportion -> String
proportionFormatter (Proportion day newFeed oldFeed) = format "{0}       {1}         {2}" [(printf "%2d." day), show newFeed, show oldFeed]

main :: IO ()
main = do
  _ <- putStrLn "day / new feed / old feed"
  let calendar = getCalendar
  _ <- traverse putStrLn (map proportionFormatter calendar)
  return ()

-- TODO
-- commands
-- show current day (day header + today's line)
-- show calendar with "you are here"
-- restart all (with confirmation
-- Free do operacji na pliku (Start? | Commit | Rollback | Restart)
