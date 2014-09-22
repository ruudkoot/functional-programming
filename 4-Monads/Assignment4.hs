{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

module Assignment4 where

import Control.Applicative (Applicative(..))
import Control.Monad       (ap, liftM, replicateM)
import Data.Foldable       (Foldable(..))
import Data.Monoid         (Monoid(..), Sum(..), (<>))
import Data.Ratio          ((%))
import System.Random       (Random(random, randomR), getStdRandom)

-- | A Game of Chance

-- * The Gambling Monad

data Coin = H | T
    deriving (Bounded, Eq, Enum, Ord, Show)

data Dice = D1 | D2 | D3 | D4 | D5 | D6
    deriving (Bounded, Eq, Enum, Ord, Show)
    
data Outcome = Win | Lose
    deriving (Eq, Ord, Show)

class Monad m => MonadGamble m where
    toss :: m Coin
    roll :: m Dice

-- Exercise 1

game :: MonadGamble m => m Outcome
game = undefined

-- * Simulation

-- Exercise 2

instance Random Coin where
    randomR (l, h) g = undefined
    random           = undefined

instance Random Dice where
    randomR (l, h) g = undefined
    random           = undefined

-- Exercise 3

instance MonadGamble IO where
    toss = undefined
    roll = undefined

-- Exercise 4

simulate :: IO Outcome -> Integer -> IO Rational
simulate = undefined

-- * Decision trees

data DecisionTree a
    = Result a
    | Decision [DecisionTree a]
    deriving (Eq, Show)

instance Functor DecisionTree where
    fmap = liftM
 
instance Applicative DecisionTree where
    pure  = return
    (<*>) = ap

-- Exercise 5

instance Monad DecisionTree where

    -- return :: a -> DecisionTree a
    return = undefined

    -- (>>=) :: DecisionTree a -> (a -> DecisionTree b) -> DecisionTree b
    (>>=) = undefined

-- Exercise 6

instance MonadGamble DecisionTree where
    toss = undefined
    roll = undefined

-- Exercise 7

probabilityOfWinning :: DecisionTree Outcome -> Rational
probabilityOfWinning = undefined

-- | Instrumented State Monad

-- Exercise 8

class Monad m => MonadState m s | m -> s where

    get :: m s
    get = undefined

    put :: s -> m ()
    put = undefined

    modify :: (s -> s) -> m s
    modify = undefined

-- * Instrumentation

data Counts = Counts {
    binds   :: Int,
    returns :: Int,
    gets    :: Int,
    puts    :: Int
} deriving (Eq, Show)

-- Exercise 9

instance Monoid Counts where
    mempty  = undefined
    mappend = undefined
    -- Note: mappend is the same as (<>) from the previous assignment. The
    --       Monoid class from the standard library requires you to call this
    --       mappend in the instance definition, but you can still use (<>)
    --       in the rest of your code.

oneBind, oneReturn, oneGet, onePut :: Counts
oneBind   = undefined
oneReturn = undefined
oneGet    = undefined
onePut    = undefined

newtype State' s a = State' { runState' :: (s, Counts) -> (a, s, Counts) }

-- Exercise 10

instance Functor (State' s) where
    fmap = liftM
 
instance Applicative (State' s) where
    pure  = return
    (<*>) = ap

instance Monad (State' s) where

    -- return :: a -> State' s a
    return x = undefined

    -- (>>=) :: State' s a -> (a -> State' s b) -> State' s b
    st >>= k = undefined

instance MonadState (State' s) s where

    -- get :: State' s s
    get = undefined

    -- put :: s -> State' s ()
    put = undefined
    
-- * Tree Labeling

data Tree a = Branch (Tree a) a (Tree a) | Leaf
    deriving (Eq, Ord, Show)

-- Exercise 11

label :: MonadState m Int => Tree a -> m (Tree (Int, a))
label = undefined

-- Exercise 12

run :: State' s a -> s -> (a, Counts)
run = undefined
