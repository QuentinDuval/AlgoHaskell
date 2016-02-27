module FunWithFold (

) where


{-
-- Example of map and fold as support for an introduction to Haskell
-}



-- Simple examples as introduction

foldIntro :: IO ()
foldIntro = do

  print $ foldl (+) 0 [1..10] -- ^ 55
  print $ foldr (+) 0 [1..10] -- ^ 55

  print $ foldl (++) "" ["use", " concat ", "insead"] -- ^ Slower
  print $ foldr (++) "" ["use", " concat ", "insead"] -- ^ Faster

  print $ foldl (-) 20 [1..5] -- ^ Expected result: -5
  print $ foldr (-) 20 [1..5] -- ^ Might not be what you think: -17


-- Applied to domain

data Transaction      = Transaction { amount :: Double }   deriving (Show)
data TransactionEvent = Unwind      { ratio :: Double }    deriving (Show)

data Balance      = Balance Double deriving (Show)
data BalanceEvent = Reset Double | Increment Double

foldDomain :: IO ()
foldDomain = do

  let apply (Transaction d) (Unwind r) = Transaction (d * r)
  print $ foldl apply (Transaction 100) [Unwind 0.5, Unwind 0.5]

  let acc (Balance t) (Reset r)     = Balance r
      acc (Balance t) (Increment i) = Balance (t + i)
  print $ foldl acc (Balance 100) [Increment 10, Reset 120, Increment 1]

-- TODO: map to do split corporate actions - Reify the actions?
-- TODO: fold to accumulate coupons?


-- Think functions!

foldFunction :: IO ()
foldFunction = do

  print $ foldl (.) id [(+5), (*2), (2-)] 1 -- ^ 7
  print $ foldr (.) id [(+5), (*2), (2-)] 1 -- ^ 7

  -- ^ Customizing the composition operator
  let comp r f i = if f i < 0 then f i else r (f i)
  print $ foldl comp id [(+5), (*2), (2-)] 1 -- ^ 7
  print $ foldl comp id [(+5), (*2), (2-)] 3 -- ^ -1

  -- ^ It is very different from simply chaining!
  let chain r f = if r < 0 then r else f r
  print $ foldl chain 1 [(+5), (*2), (2-)] -- ^ -10


-- State machine

data FSM = Init | Running Int | Stopped Int deriving (Show)
data Transition = Start | Task | Stop       deriving (Show)

fsm :: FSM
fsm = Init

handle :: FSM -> Transition -> FSM
handle Init Start       = Running 0
handle (Running s) Task = Running (succ s)
handle (Running s) Stop = Stopped s
handle state _          = state

-- fsm = loop start
--   where
--     loop handler tr
--       = case handler tr
--           of Just next -> loop next
--              _         -> ()
--
--     start Start = run
--     run   Stop  = stop
--     stop  _     = stop

foldFSM :: IO ()
foldFSM = do
  print $ foldl handle Init [Start, Task, Task, Stop, Task]
