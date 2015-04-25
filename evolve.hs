import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

data Organism  = Organism { getAlive  :: Bool,
                            getEnergy :: Int,
                            getDirec  :: Int,
                            getGenes  :: [Int] }
                 deriving (Show)
type Point     = (Int, Int)
type DataPoint = (Point, Organism)
type World     = [DataPoint] -- could this be a map or set?
type Time      = [World] -- snapshots of the world over time

fstDP :: DataPoint
fstA :: Organism
mid :: Point
ht :: Int
wd :: Int
pe :: Int
w :: World

ht = 30
mid = (div ht 2, div wd 2) :: Point
fstA = Organism True 1000 0 [1,2,3,4,5,6,7,8] --figure out rand for this
fstDP = (mid, fstA)
wd = 100
pe = 80
w = [fstDP]

pointList :: World -> [Point]
pointList w = map fst w

animalList :: World -> [Organism]
animalList w = map snd w

makeWorld :: [Point] -> [Organism] -> World
makeWorld p o = zip p o

moveAnimal :: Point -> Point
moveAnimal p =
    let newX = (fst p) + 1
        newY = (snd p) + 1
    in (newX, newY)

addAnimal :: DataPoint -> World -> World
addAnimal dp w = dp:w

-- this needs to keep the points - not just zip them back together. . .
removeDead :: World -> World
removeDead w = 
    let wO = animalList w
        wP = pointList w
        nD = filter ((<) 100 . getEnergy) wO
        newW = makeWorld wP nD
    in newW


eatAnimal :: Organism -> Organism
eatAnimal hungryA =
    let fullA = Organism (getAlive hungryA)
                         (getEnergy hungryA + pe)
                         (getDirection hungryA)
                         (getGenes hungryA)
    in fullA

turnAnimal :: Organism -> Organism
turnAnimal a =
    let turnedA = Organism (getAlive a)
                           (getEnergy a)
                           (getDirection a + 1)
                           (getGenes a)
    in turnedA

reproduceAnimal :: Organism -> Organism
reproduceAnimal a =
        let repA = Organism (getAlive a)
                            (getEnergy a `div` 2)
                            (getDirection a)
                            (getGenes a)
        in repA

animalEvents :: DataPoint -> DataPoint
animalEvents dp  =
        let o = turnAnimal . reproduceAnimal . eatAnimal . snd $ dp
        in (moveAnimal $ fst dp, o) :: DataPoint

addAnimalDoStuff :: World -> World
addAnimalDoStuff wld = 
    let newX = (fst (fst $ wld!!0)) + 1
        newY = (snd (fst $ wld!!0))
        newA = Organism True
                        1000
                        (getDirection (snd $ wld!!0) + 1)
                        (getGenes (snd $ wld!!0))
        newDP = ((newX, newY), newA) :: DataPoint
        addA = addAnimal newDP wld
        newW = map animalEvents addA
    in newW
    
overTime :: [World] -> [World]
overTime wl = 
    if (length wl < 10)
        then let newWL = map addAnimalDoStuff wl
             in overTime newWL ++ wl
    else wl




showMenu :: IO ()
showMenu = do
    putStr "\n\t...:::MENU:::...\n"
    putStrLn "\t1. Show Animal\n\t2. Move Animal\n\t3. Add Plant\n\t4. Simulate Day"
    putStrLn "\t----------------\n"

showPoint :: Point -> IO ()
showPoint p =  do
    putStrLn $ "\nThe current location is: " ++ show p

showOrganism :: Organism -> IO ()
showOrganism o = do
    let a = show $ getAlive o
        e = show $ getEnergy o
        d = show $ getDirection o
    putStrLn $ "Alive: " ++ a
    putStrLn $ "Energy: " ++ e
    putStrLn $ "Direction: " ++ d
    


main = forever $ do
    showMenu

    putStrLn "Make a choice: "
    choiceAsString <- getLine
    putStrLn "Enter an index: "
    indexString <- getLine

    let choice = read choiceAsString :: Int
    let index  = read indexString :: Int

    if choice == 1
        then do
            putStrLn "Show Index"
            -- showDataPoint index
    else if choice == 2
        then do
            putStrLn "Move Animal"
    else if choice == 3
        then do
            putStrLn "Add Plant"
    else if choice == 4
        then do
            putStrLn "Simulate Day"
    else
        putStrLn "That's not a choice."
