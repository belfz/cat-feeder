module Util
  (
    getCommitDayNumber
  , getLastDayNumber
  ) where

import Text.Read

lastOption :: [a] -> Maybe a
lastOption list = case (reverse list) of
  (x:_) -> Just x
  _     -> Nothing

getCommitDayNumber :: [String] -> Int
getCommitDayNumber list = case ((lastOption list) >>= readMaybe) of
  (Just day) -> day + 1
  Nothing    -> 1

getLastDayNumber :: [String] -> Int
getLastDayNumber list = case ((lastOption list) >>= readMaybe) of
  (Just day) -> day
  Nothing    -> 1
