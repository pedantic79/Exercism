module Clock
  ( clockHour
  , clockMin
  , fromHourMin
  , toString
  ) where

import           Text.Printf (printf)

newtype Clock =
  Clock Minutes
  deriving (Eq)

type Minutes = Int

type Hours = Int

clockHour :: Clock -> Hours
clockHour (Clock minutes) = minutes `div` 60

clockMin :: Clock -> Minutes
clockMin (Clock minutes) = minutes `mod` 60

fromHourMin :: Hours -> Minutes -> Clock
fromHourMin hour min = fromInteger . fromIntegral $ hour * 60 + min

toString :: Clock -> String
toString = show

instance Show Clock where
  show c = printf "%02d:%02d" (clockHour c) (clockMin c)

instance Num Clock where
  (Clock a) + (Clock b) = fromInteger . fromIntegral $ a + b
  (Clock a) * (Clock b) = Clock $ a * b
  abs c = signum c * c
  fromInteger = Clock . normalize . fromIntegral
  negate (Clock a) = Clock (1440 - a)
  signum (Clock a)
    | a < 0 = Clock (-1)
    | a > 0 = Clock 1
    | otherwise = Clock 0

normalize :: Minutes -> Minutes
normalize m = (m `mod` 1440 + 1440) `mod` 1440
