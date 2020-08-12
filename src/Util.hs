module Util
  (
    getLastDayNumber
  ) where

import Text.Read

lastOption :: [a] -> Maybe a
lastOption list = case (reverse list) of
  (x:_) -> Just x
  _     -> Nothing

getLastDayNumber :: [String] -> Int
getLastDayNumber list = case ((lastOption list) >>= readMaybe) of
  (Just day) -> day
  Nothing    -> 1
