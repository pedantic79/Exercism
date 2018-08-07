module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = (test 400 || not (test 100)) && test 4
    where test = (0 ==) . mod year
