{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module FunWithFold (

) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Par
import Control.Parallel.Strategies
import Data.Char
import Data.Function((&))
import Data.List
import qualified Data.Map as M
import System.IO


--------------------------------------------------------------------------------
-- Example of composition as support for an introduction to Haskell
--------------------------------------------------------------------------------

composition :: IO ()
composition = do

  -- ^ Useless return 10 function
  let useless1 = length . replicate 10
  print $ useless1 'c'

  -- ^ Useless return first element of pair function
  let useless2 = length . uncurry replicate
  print $ useless2 (10, 'c')

  -- ^ Useless return first argument function
  let (.&) = (.).(.)
      useless3 = length .& replicate
  print $ useless3 10 'c'



--------------------------------------------------------------------------------
-- Example of algebraic data types and functional thinking
--------------------------------------------------------------------------------

data JSON
  = JInt Int
  | JFloat Double
  | JBool Bool
  | JString String
  | JList [JSON]
  | JObject [(String, JSON)]
  deriving (Show)

surround :: String -> String -> String
surround (l:r:_) s = [l] ++ s ++ [r]
withQuote = surround "\"\""

formatJson :: JSON -> String
formatJson (JInt i)     = withQuote (show i)
formatJson (JFloat d)   = withQuote (show d)
formatJson (JBool b)    = withQuote (show b)
formatJson (JString s)  = withQuote s
formatJson (JList js)   = fmap formatJson js
                            & intersperse ","
                            & concat
                            & surround "[]"
formatJson (JObject os) = fmap (withQuote *** formatJson) os
                            & fmap (\(k,v) -> k ++ ":" ++ v)
                            & intersperse ","
                            & concat
                            & surround "{}"


adtIntro :: IO ()
adtIntro = do
  let json = JObject [
                ("name" , JString "Kurt"),
                ("age"  , JInt 27),
                ("dead?", JBool True),
                ("songs", JList [ JString "Lithium", JString "Come as you are" ])
              ]
  putStrLn (formatJson json)


--------------------------------------------------------------------------------
-- Example of map as support for an introduction to Haskell
--------------------------------------------------------------------------------

mapIntro :: IO ()
mapIntro = do

  print $ map (+1) [1..10]
  print $ map toUpper "teSt"    -- ^ "TEST"
  print $ map digitToInt "9173" -- ^ [9, 1, 7, 3]

  let commands = map mod [1..5] -- ^ A stream transformed to commands
  print $ map ($5) commands     -- ^ [1, 2, 3, 4, 0]


mapNested :: IO ()
mapNested = do

  print $ (fmap . fmap) (+1) [Just 1, Nothing, Just 2]
  print $ (fmap . fmap) (+1) (Just [1, 2])

  let l = [Just 2, Just 3, Just 4]
  let m = M.fromList [(2, [2, 4, 6]), (3, [3, 6, 9])]
  print $ (fmap . fmap) intToDigit l  -- ^ [Just '2', Just '3', Just '4']
  print $ (fmap . fmap) intToDigit m  -- ^ fromList [(2, "246"), (3, "369")]


mapFrequencies :: IO ()
mapFrequencies = do
  let inputs = [1] ++ replicate 100 2 ++ concatMap (replicate 10 )[3, 4]
      freqs = foldr (\k -> M.insertWith (+) k 1) M.empty inputs
  -- ^ A pretty standard formula for IR systems
  print $ fmap (\i -> 1 + log i) freqs


--------------------------------------------------------------------------------
-- Generalization of map
--------------------------------------------------------------------------------

-- | Would be too slow : Just fmap over the STM instead
-- mapQueue :: (a -> b) -> TQueue a -> IO (TQueue b)
-- mapQueue f i = do
--   o <- newTQueueIO
--   forkIO $ forever $ atomically $ do
--     v <- readTQueue i
--     writeTQueue o (f v)
--   return o
--
-- filterQueue :: (a -> Bool) -> STM a -> IO (TQueue a)
-- filterQueue cond i = do
--   o <- newTQueueIO
--   forkIO $ forever $ atomically $ do
--     v <- i
--     when (cond v) $ writeTQueue o v
--   return o

filterChan :: (Monad m) => (a -> Bool) -> m a -> m a
filterChan cond a = do
  v <- a          -- ^ Beware: calling "a" twice means you read twice
  if cond v       -- ^ In general, it means triggering the side effect twice
    then pure v
    else filterChan cond a

altChan :: STM a -> STM a -> STM a
altChan = orElse

mappingStreams :: IO ()
mappingStreams = do

  ints <- newTBQueueIO 1
  strs <- newTBQueueIO 1

  -- Beware: atomically is needed after merge chan, or filtering will block!
  -- * It would try to read atomically until it finds a good input
  -- * And would put back the elements in the queue if none were found
  let lhs = readTBQueue ints                      -- ^ Simple queue output
      rhs = readTBQueue strs & fmap digitToInt    -- ^ Transform the queue output
      out = atomically (altChan lhs rhs)          -- ^ Merging inputs
              & filterChan even                   -- ^ Filtering the outputs
              & fmap intToDigit                   -- ^ Transform further

  let writeLhs = writeTBQueue ints                -- ^ Simple queue input
      writeRhs = writeTBQueue strs . intToDigit   -- ^ Transform the queue input

  r <- async $ replicateM_ 10 $ do
      v <- out
      print v

  forM_ [0..9] $ \i ->
    atomically (writeLhs i >> writeRhs i)         -- ^ Atomically forces alternation here
  wait r


--------------------------------------------------------------------------------
-- To communicating sequence process
--------------------------------------------------------------------------------

data Screen = Screen { name, mail :: TVar String }

makeScreen :: STM Screen
makeScreen = Screen <$> newTVar "" <*> newTVar ""

controler :: Screen -> TMVar () -> IO ()
controler Screen{..} okChan =
  do print "Waiting user inputs..."
     loop
     print "Waiting confirmation..."
     atomically $ takeTMVar okChan
     print "Confirmation received..."
  where
    loop = do
      continue <- atomically $ do
        takeTMVar okChan
        n <- readTVar name
        m <- readTVar mail
        pure (null n || null m)
      when continue $ print "Missing info!" >> loop

cspLike :: IO ()
cspLike = do

  screen <- atomically makeScreen
  okChan <- newEmptyTMVarIO
  r <- async (controler screen okChan)

  atomically $ putTMVar okChan ()
  atomically $ writeTVar (name screen) "John Doe"
  atomically $ putTMVar okChan ()
  atomically $ writeTVar (mail screen) "mail@mail.mail"
  atomically $ putTMVar okChan ()
  atomically $ putTMVar okChan ()
  wait r >> print "Close screen"


--------------------------------------------------------------------------------
-- Example of fold as support for an introduction to Haskell
--------------------------------------------------------------------------------

foldIntro :: IO ()
foldIntro = do

  print $ foldl (+) 0 [1..10] -- ^ 55
  print $ foldr (+) 0 [1..10] -- ^ 55

  print $ foldl (++) "" ["use", " concat ", "instead"] -- ^ Slower
  print $ foldr (++) "" ["use", " concat ", "instead"] -- ^ Faster

  print $ foldl (-) 20 [1..5] -- ^ Expected result: -5
  print $ foldr (-) 20 [1..5] -- ^ Might not be what you think: -17

  let groupWith proj = foldr (\a -> M.insertWith (++) (proj a) [a]) M.empty
  print $ fmap sum (groupWith (`mod` 5) [1..100])


foldFunction :: IO ()
foldFunction = do

  print $ foldl (.) id [(+5), (*2), (2-)] 1 -- ^ 7
  print $ foldr (.) id [(+5), (*2), (2-)] 1 -- ^ 7

  -- ^ Customizing the composition operator
  let comp r f i = if f i < 0 then f i else r (f i)
  print $ foldl comp id [(+5), (*2), (2-)] 1 -- ^ 7
  print $ foldr comp id [(+5), (*2), (2-)] 3 -- ^ -1

  -- ^ It is very different from simply chaining!
  let chain r f = if r < 0 then r else f r
  print $ foldl chain 1 [(+5), (*2), (2-)] -- ^ -10

  -- ^ Comparison, stopping early (lexicographic compare)
  let lexicoComp = foldr (\(a, b) rest ->
                      let cab = compare a b
                      in if cab /= EQ then cab else rest) EQ
  print $ lexicoComp $ zip [1, 2, 3] [1, 3, 2]
  print $ lexicoComp $ zip [1, 3, 2] [1, 2, 3]
  print $ lexicoComp $ zip [1..] [2..]


--------------------------------------------------------------------------------
-- Applied to domain
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- State machine
--------------------------------------------------------------------------------

data Transition = Start | Task | Stop
  deriving (Show)

data State = State {
  state   :: Int,
  accept  :: Transition -> State
}

start :: State
start = State 0 f where
  f Start = running 0
  f _     = start

running :: Int -> State
running v = State v f where
  f Task = running (succ v)
  f Stop = stopped v
  f _    = running v

stopped :: Int -> State
stopped v = State v (const $ stopped v)

foldFSM :: IO ()
foldFSM = do
  let s = foldl accept start [Start, Task, Task, Stop, Task]
  print (state s)


--------------------------------------------------------------------------------
-- Higher order function and explaining RAII equivalent
--------------------------------------------------------------------------------

class (Monad m) => LicenceMonad m where
  checkLicence :: String -> m Bool

instance LicenceMonad IO where
  checkLicence _ = pure True

withLicence :: (LicenceMonad m) => String -> m a -> m (Maybe a)
withLicence licenceName fct = do
  granted <- checkLicence licenceName
  if granted
    then fmap Just fct
    else pure Nothing

withTransaction :: STM a -> IO a
withTransaction = atomically

withReadFile :: FilePath -> (Handle -> IO a) -> IO a
withReadFile path fct = do
  f <- openFile path ReadMode
  r <- fct f
  hClose f
  return r


--------------------------------------------------------------------------------
-- Phantom types
--------------------------------------------------------------------------------

data Status = Edited | Validated

data Trade (s :: Status) = Trade {
  quantity    :: Double,
  instrument  :: String
} deriving (Show)

newTrade :: Double -> String -> Trade Edited
newTrade = Trade

setQuantity :: Trade a -> Double -> Trade Edited
setQuantity t q = t { quantity = q }

validateTrade :: Trade a -> Trade Validated
validateTrade t = t { quantity = quantity t }

saveTrade :: Trade Validated -> IO ()
saveTrade t = putStrLn ("[SAVING] " ++ show t)

testPhantomType :: IO ()
testPhantomType = do
  let t1 = newTrade 10 "EUR"
      t2 = validateTrade t1
      t3 = setQuantity t2 12
      t4 = validateTrade t3
  saveTrade t2 -- ^ Saving t1 would not compile
  saveTrade t4 -- ^ Saving t3 would not compile


--------------------------------------------------------------------------------
-- Think functionally
--------------------------------------------------------------------------------

roundRobin :: [[a]] -> [a]
roundRobin = go []
  where
    go acc ([]: rest)      = go acc rest
    go acc ((x:xs) : rest) = x : go (xs : acc) rest
    go []  []              = []
    go acc []              = go [] (reverse acc)

roundRobin2 :: [[a]] -> [a]
roundRobin2 = concat . transpose


--------------------------------------------------------------------------------
-- Example of parallel computations
--------------------------------------------------------------------------------

fibPar :: Int -> Par Integer
fibPar 0 = pure 0
fibPar 1 = pure 1
fibPar n = do
  a <- fibPar (n-1)
  b <- fibPar (n-2)
  return (a + b)

parallelTest :: IO ()
parallelTest = do

  let r = sum (map (+1) [1..1000] `using` parListChunk 100 rpar)
  print r

  let f = runPar (fibPar 20)
  print f


--------------------------------------------------------------------------------
-- Example of continuations
--------------------------------------------------------------------------------

-- Amazing post:
-- http://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work
--
--
-- The type of a continuation:
--
-- :t flip ($) True :: (Bool -> a) -> a
-- :t (&) True      :: (Bool -> a) -> a
-- :t (True &)      :: (Bool -> a) -> a
--
-- The definition of the continuation monad:
-- * Given a partial computation from 'a' to 'r'
-- * A transformation from 'a' to a partial computation from 'b' to 'r'
-- * Provides a partial computation from 'b' to 'r'
-- * Basically complete part of the missing computation
-- * So it looks like a transformation of the kind (a -> r) to (b -> r)
--
-- newtype Cont r a = Cont { runCont :: (a -> r) -> r }
-- instance Monad (Cont r) where
--   return :: a -> Cont r a -- Same as: a -> ((a -> b) -> b)
--   return a = Cont ($ a)
--   >>= :: Cont r a -> (a -> Cont r b) -> Cont r b
--   m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

contFun1 :: Cont String Int
contFun1 = do

  -- Uncomment the second line to ignore the outer computation
  a <- return 1
  -- a <- cont (const "toto")

  -- ^ All these formulations are equivalent:
  -- b <- return 10
  -- b <- cont $ \c -> c 10
  -- b <- cont (10 &)
  b <- cont ($ 10)

  -- Uncomment the following line to ignore the outer computation
  -- b <- cont $ \c -> "toto"
  return (a + b)

contFun2 :: (Int -> String) -> String
contFun2 = undefined

contTest :: IO ()
contTest = do
  print $ runCont contFun1 show
  print $ runCont contFun1 (const "Ignore at last step")
