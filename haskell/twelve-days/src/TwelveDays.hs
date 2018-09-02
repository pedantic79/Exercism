module TwelveDays (recite) where

import Data.List (intersperse)
import Text.Printf (printf)

recite :: Int -> Int -> [String]
recite start stop = map verse [start..stop]

verse :: Int -> String
verse n = printf "On the %s day of Christmas my true love gave to me, %s." day prs
    where
        day = ordinal (n - 1)
        prs = presents n

presents :: Int -> String
presents n
    | n == 1    = head gifts
    | otherwise = concat $ (intersperse ", " . reverse . tail . take n $ gifts) ++ [", and ", head gifts]

gifts :: [String]
gifts = [ "a Partridge in a Pear Tree"
        , "two Turtle Doves"
        , "three French Hens"
        , "four Calling Birds"
        , "five Gold Rings"
        , "six Geese-a-Laying"
        , "seven Swans-a-Swimming"
        , "eight Maids-a-Milking"
        , "nine Ladies Dancing"
        , "ten Lords-a-Leaping"
        , "eleven Pipers Piping"
        , "twelve Drummers Drumming"
        ]

ordinal :: Int -> String
ordinal = (ordinalNumber !!)

ordinalNumber :: [String]
ordinalNumber = [ "first"
                , "second"
                , "third"
                , "fourth"
                , "fifth"
                , "sixth"
                , "seventh"
                , "eighth"
                , "ninth"
                , "tenth"
                , "eleventh"
                , "twelfth"
                ]
