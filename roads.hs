import Data.List

-- Define a Secion: each section has three values, an a path , a b path,
-- and a c (cross) path 
-- Also define a RoadSystem Typeclass to be a list of Sections
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
{-
                A----50---(A1)---5----(A2)---40---(A3)---10---(A4)
                            |          |           |           |
                           30         20           25          0
                            |          |           |           |
                B----10---(B1)---90---(B2)---2----(B3)---8----(B4)
-}
-- Define a Label, which is a lot like an enum (those values A,B,C are now "Labels")
-- Then define a Path Typeclass to be a list of Label, Int pairs
-- This is all intuitive: we are describing what the model shows.
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- RoadStep takes as arguments a pair of paths and a Section. The first time
-- we will pass ([],[]) in, because at the start we havent gone anywhere.
-- In the end we return a pair of paths appened onto the original ones.
-- Possible returns could have lists with one (Path,Path) pair or two, depending
-- on if a c (cross) path was taken.  These are the only things that can be returned.
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
-- use let to declare a bunch of variables

    -- the first two are the current path sums (the snd of the (Label, Int) pairs
    -- we dont want fst because that's just a Label
    let priceA = sum $ map snd pathA                        -- 0
        priceB = sum $ map snd pathB                        -- 0
        
    -- the cost of moving directly forward on the A path
    -- is equal to the current sum plus the a section value
    -- the cross cost starts on b and moves up the c (cross)
        forwardPriceToA = priceA + a                        -- 0 + 50
        crossPriceToA   = priceB + b + c                    -- 0 + 10 + 30
    
    -- just like a, we can either move directly, or start on a and move up c
        forwardPriceToB = priceB + b                        -- 0 + 10
        crossPriceToB   = priceA + a + c                    -- 0 + 50 + 30
    
    -- to get the new values (that will be returned), we compare the results
    -- of direct vs indirect travel
        newPathToA = if forwardPriceToA <= crossPriceToA    -- 50 <= 40 ?
                        then (A,a):pathA                    -- (A,50):[]
                        else (C,c):(B,b):pathB              -- (C,30):(B,10):[]
                        
        newPathToB = if forwardPriceToB <= crossPriceToB    -- 10 <= 80 ?
                        then (B,b):pathB                    -- (B,10):[]
                        else (C,c):(A,a):pathA              -- (C,30):(A,50):[]
                        
    in (newPathToA, newPathToB)

-- The optimal path just takes the previous function roadStep and maps
-- it over the RoadSystem (which is just a list of Sections
-- it returns the complete path that is the shortest (reversed because it conses
-- new items on the beginning, listing them the wrong way in the end)
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath

-- Now we need to read a textual representation of a road system from std input,
-- covert it to a type of RoadSystem, run it through optimalPath, then print the path

-- groupsOf takes a number n and a list lst and breaks the list into groups of size n
-- For n = 0 it is undefined (because that doesnt make sense)
-- for n and [] it returns [], the base case (for recursion)
-- and n xs takes n from xs and conses that with groupsOf n and xs with n dropped off.
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)


main = do
    contents <- getContents
    let threes     = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path       = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice  = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice