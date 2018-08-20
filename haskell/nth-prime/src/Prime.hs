module Prime (nth) where
import Control.Monad (join)

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ prime !! (n - 1)

prime :: [Integer]
prime = 2:3:[ x | k<-[1..], r<-[-1, 1], let x=6*k+r, all ((0 /=) . (x `mod`)) . takeWhile ((<= x) . join (*)) $ prime]
