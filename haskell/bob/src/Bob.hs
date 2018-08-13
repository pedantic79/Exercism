module Bob (responseFor) where
import Data.Char (isLower, isSpace, isUpper)

responseFor :: String -> String
responseFor xs
    | xs' == ""            = "Fine. Be that way!"
    | isYell && isQuestion = "Calm down, I know what I'm doing!"
    | isYell               = "Whoa, chill out!"
    | isQuestion           = "Sure."
    | otherwise            = "Whatever."
    where
        isQuestion = (== '?') . last $ xs'
        isYell     = all ($ xs') [any isUpper, not . any isLower]
        xs'        = filter (not . isSpace) xs
