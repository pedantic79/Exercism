module Beer (song) where

import Text.Printf (printf)

song :: String
song = concatMap verse [99, 98..0]

verse :: Int -> String
verse 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
          \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
verse n = printf "%s of beer on the wall, %s of beer.\n\
                 \Take %s down and pass it around, %s of beer on the wall.\n\n"
                 bot bot (itone n) next
    where
        bot = bottles n
        next = bottles $ n - 1

itone :: Int -> String
itone 1 = "it"
itone _ = "one"

bottles :: Int -> String
bottles 0 = "no more bottles"
bottles 1 = "1 bottle"
bottles n = show n ++ " bottles"
