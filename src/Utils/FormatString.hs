module Utils.FormatString where


-- | Pad and center strings

padR, padL, center :: Int -> String -> String
padR n s    = take n (s ++ repeat ' ')
padL n s    = replicate (n - length s) ' ' ++ s
center n s  = padR n (replicate pl ' ' ++ s)
  where
    p  = n - length s
    pl = p - p `div` 2
