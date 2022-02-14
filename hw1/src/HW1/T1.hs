module HW1.T1
    ( Day(..)
    , nextDay
    , afterDays
    , isWeekend
    , daysToParty
    ) where

import Numeric.Natural as Natural    


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

nextDay :: Day -> Day
nextDay day = case day of
    Monday -> Tuesday
    Tuesday -> Wednesday
    Wednesday -> Thursday
    Thursday -> Friday
    Friday -> Saturday
    Saturday -> Sunday
    Sunday -> Monday

afterDays :: Natural -> Day -> Day    
afterDays 0 day = day 
afterDays i day = afterDays (i - 1) $ nextDay day

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day = 1 + daysToParty (nextDay day)
