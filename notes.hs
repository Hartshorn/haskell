-- the list all (x,y) points on the map
world :: [(Int, Int)]
world = [(x, y) | x <- [0..100], y <- [0..30]]


-- takes a list of things that are orderable and returns the largest
-- empty list is an error
-- base case (one value) is just the value
-- other wise compare the first element to the recursive rest of the list
-- use the MAX function (takes two numbres, returns the larger)
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- takes something that is a number (i) and is orderable and something else (a)
-- and return a list of i number of a repitions.
-- i = 0 is zero repitions (base case), so just an empty list
-- otherwise recurse with a cons of a and the function called with i - 1
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' i a
    | i <= 0 = []
    | otherwise = a : replicate' (i - 1) a

-- take a number of items (n) from a list [a]
-- for the case of n and anything - if n i zero, return an empty list
-- (it doesnt matter what the other thing is a list of, n = 0 will return [])
-- the case of any number n and an empty list returns [].
-- the case of n > 0 and a non empty list will give the first element consed
-- with the recursed function with n - 1 and the rest of the list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ []   = []
take' n (x:xs) = x : take' (n - 1) xs

-- reverse is simple - take a list and return a list
-- for an empty list, return an empty list
-- everything else is just the recursed rest with the head added on at the end
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

-- repeat is infinite - take something and make a list of it forever
-- sounds scary, but lazy evaluation means it's pretty great
-- (try "take 5 $ repeat 3" - you'll get [3,3,3,3,3])
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- zip will take two lists and turn them into a list of pairs of each element
-- a list of tuples (sounds better, right?)
-- they can be lists of anything
-- edges are empty lists - if a or b is, return an empty list
-- otherwise, just make a pair of the two heads and cons it with 
-- a recursion over the rests
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- elem will check if a (of the Eq typeclass) is in a list of the same type of elements
-- it returns a boolean (True / False)
-- so base case is something and an empty list, which is False
-- otherwise check if the head of the list is equal to the item, if False,
-- recurse over the rest of the list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | x == a = True
    | otherwise = elem' a xs
-- using foldl
-- elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
    
-- The mandatory Quicksort Function!
-- takes a list of a (of Ord Typeclass) and sorts it
-- base case - empty list returns an empty list
-- otherwise we describe what we want:
-- a quicksorted list is equal to 
-- the sorted less than head elements
-- the head
-- the sorted greater than the head elements
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort . filter (<= x) $ xs) ++ [x] ++ (qsort . filter (> x) $ xs)

-- how about the product function?  make it up!
-- the product of an empty list is 1
product' :: (Num i) => [i] -> i
product' [] = 1
product' (x:xs) = x * product' xs

-- zipWith takes a function and two lists and applies the function to the two
-- returning a list of the new elements
-- so zipWith + [1,1] [1,2] would give [2,3]
-- the base case is either list is empty, then just return an empty list
-- the type definition says (a -> b -> c)
-- which is just a function that takes two thing and returns a another
-- we need it to be separate from the rest of the definition to show that it
-- is a function. This will be represented as f (x:xs) (y:ys), where
-- f is (a -> b -> c)
-- (x:xs) is [a]
-- (y:ys) is [b]
-- and the return value is [c]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


-- Do something crazy - find the largest number below 100,000 that is
-- divisible by 3829
-- why? because functional
-- just get the head of a filtered list of all numbers below 100,000.
-- filtered on what?  mod 3829.
-- filter usually looks like "filter (<3) [1,2,3]" and returns [1,2]
-- that just looks at each value and finds if it is true that it is less than 3
-- then makes a list of all values that are.
-- You might think that filter (<3) returning a list is a bad idea in our case
-- We dont need a long list of all numbers below 100,000, right?  Here comes
-- lazy evaluation once again - because we are taking the head, it stops at
-- the first element it finds, knowing it doesnt need to find the rest that satisfy
-- the filter.
-- also, lambdas are great. \!  
largestDivisible :: (Integral a) => a
largestDivisible = head (filter (\x -> mod x 3829 == 0) [100000, 99999..])

-- To see how many elements it takes for the sum of the roots of all natural 
-- to exceed 1000,
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- Sum using foldl - take a list and add up the elements
-- could be the first way - better the second way
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0