module Queue.Utils where

import Queue.Class


consume :: (IQueue q) => (a -> b -> b) -> b -> q a -> b
consume f init q
    | isNull q  = init
    | otherwise = consume f (f (top q) init) (pop q)

