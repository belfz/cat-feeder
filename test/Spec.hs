import Test.HUnit
import Feeder

expectedCalendar :: Calendar
expectedCalendar = [
  Proportion {day = 1, newFeed = 1, oldFeed = 6},
  Proportion {day = 2, newFeed = 1, oldFeed = 6},
  Proportion {day = 3, newFeed = 1, oldFeed = 6},
  Proportion {day = 4, newFeed = 1, oldFeed = 6},
  Proportion {day = 5, newFeed = 1, oldFeed = 6},
  Proportion {day = 6, newFeed = 2, oldFeed = 5},
  Proportion {day = 7, newFeed = 2, oldFeed = 5},
  Proportion {day = 8, newFeed = 2, oldFeed = 5},
  Proportion {day = 9, newFeed = 2, oldFeed = 5},
  Proportion {day = 10, newFeed = 2, oldFeed = 5},
  Proportion {day = 11, newFeed = 3, oldFeed = 4},
  Proportion {day = 12, newFeed = 3, oldFeed = 4},
  Proportion {day = 13, newFeed = 3, oldFeed = 4},
  Proportion {day = 14, newFeed = 3, oldFeed = 4},
  Proportion {day = 15, newFeed = 3, oldFeed = 4},
  Proportion {day = 16, newFeed = 4, oldFeed = 3},
  Proportion {day = 17, newFeed = 4, oldFeed = 3},
  Proportion {day = 18, newFeed = 4, oldFeed = 3},
  Proportion {day = 19, newFeed = 4, oldFeed = 3},
  Proportion {day = 20, newFeed = 4, oldFeed = 3},
  Proportion {day = 21, newFeed = 5, oldFeed = 2},
  Proportion {day = 22, newFeed = 5, oldFeed = 2},
  Proportion {day = 23, newFeed = 5, oldFeed = 2},
  Proportion {day = 24, newFeed = 5, oldFeed = 2},
  Proportion {day = 25, newFeed = 5, oldFeed = 2},
  Proportion {day = 26, newFeed = 6, oldFeed = 1},
  Proportion {day = 27, newFeed = 6, oldFeed = 1},
  Proportion {day = 28, newFeed = 6, oldFeed = 1},
  Proportion {day = 29, newFeed = 6, oldFeed = 1},
  Proportion {day = 30, newFeed = 6, oldFeed = 1},
  Proportion {day = 31, newFeed = 7, oldFeed = 0}]

calendarTest :: Test
calendarTest = TestCase (assertEqual "the calendar" (getCalendar) expectedCalendar)

main :: IO ()
main = do
  runTestTT $ TestList [TestLabel "should be up to expectations" calendarTest]
  return ()
