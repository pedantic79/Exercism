module Bob (responseFor) where
import Data.Char (isSpace, toLower, toUpper)

responseFor :: String -> String
responseFor xs
    | xs' == ""                = "Fine. Be that way!"
    | isYell && isQuestion xs' = "Calm down, I know what I'm doing!"
    | isYell                   = "Whoa, chill out!"
    | isQuestion xs'           = "Sure."
    | otherwise                = "Whatever."
    where
        isQuestion [] = False
        isQuestion ss = last ss == '?'
        isYell        = xsUpper == xs' && xsUpper /= xsLower
        xsUpper       = map toUpper xs'
        xsLower       = map toLower xs'
        xs'           = filter (not . isSpace) xs
