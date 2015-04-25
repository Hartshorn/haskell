import Data.List
import Data.Char
import Control.Applicative

{- for a system like:

        Heads <-> Tails
    
-}
data Omega = Heads | Tails deriving (Ord, Eq, Show)

-- Time is an infinite list of [Head, Tails] values repeating
-- It is non random, but let's us "take" sequences to play with
-- Usage is: (let ten = take 10 time) and then (map f1 ten)
-- From there we can index out locations or sections to see what happened
time :: [Omega]
time = intersperse Heads . repeat $ Tails

-- f0 is nothing changes: Heads -> Heads (coin glued to the table)
-- id function is the same as (\x -> x) or f x = x
f0 :: Omega -> Omega
f0 = id

-- f1 is a circle: Tails -> Heads -> Tails . . .
f1 :: Omega -> Omega
f1 x = case x of
        Tails -> Heads
        Heads -> Tails

-- Give a function and an index, get back value.
-- Flipped 5000 times? f = f1, i = 5000
whatHappened :: (Omega -> Omega) -> Int -> Omega
whatHappened f i = (!!i) . map f . take (i + 1) $ time

{-
    if f(t) = t^2, what is the derivative?
    
    Derivative is defined as the change in f over the chang in t.
    df(t) / dt
    
    df is f(t + dt) - f(t)
    f(t + dt) = (t + dt)^2 = t^2 + 2tdt + dt^2
    f(t) = t^2
    
    SO
    f(t + dt) - f(t) = t^2 + 2tdt + dt^2 - t^2
    
    WHICH BECOMES
    2tdt + dt^2
    
    GIVEN ALL OF THAT
    df(t) / dt = 2tdt + dt^2 / dt = dt(2t + dt) / dt = 2t + dt 
    
    AND AS t -> 0 ... df(t)/dt = 2t
-}

f3 :: Integer -> Integer
f3 t = t^2

df3 :: Integer -> Integer
df3 t = 2 * t


