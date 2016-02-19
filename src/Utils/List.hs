module Utils.List where


rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) $ drop n $ cycle xs
