module Feeder
  (
    getCalendar
  , Proportion (..)
  , Calendar
  ) where

type NewFeed = Int
type OldFeed = Int

data Proportion = Proportion {
  day :: Int
  , newFeed :: NewFeed
  , oldFeed :: OldFeed
} deriving (Eq, Show)

type Calendar = [Proportion]

incProportionDay :: Proportion -> Proportion
incProportionDay (Proportion day newFeed oldFeed) = Proportion (day + 1) newFeed oldFeed

startingPoint :: Proportion
startingPoint = Proportion 1 1 6

reducer :: Calendar -> Int -> Calendar
reducer acc forDay =
        let last = head acc
        in (calculateProportion forDay last) : acc

getCalendar :: Calendar
getCalendar = 
  let
    zero = [startingPoint]
    range = [1..30]
    in reverse $ foldl reducer zero range

calculateProportion :: Int -> Proportion -> Proportion
calculateProportion forDay proportion = 
  let opt = forDay `mod` 5 in
    case opt of
      0 -> incProportionDay $ Proportion (day proportion) ((+1) $ newFeed proportion ) ( (oldFeed proportion) - 1)
      _ -> incProportionDay $ proportion
