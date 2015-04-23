import Control.Monad
import qualified Data.Set as Set

data Organism  = Organism { getAlive :: Bool, 
                            getEnergy :: Int, 
                            getDirection :: Int, 
                            getGenes :: [Int] } 
                 deriving (Show)
type Point     = (Int, Int)
type DataPoint = (Point, Organism)
type World     = [DataPoint] -- could this be a map or set?

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

pointList :: [Point]
pointList = map fst w

animalList :: [Organism]
animalList = map snd w

moveAnimal :: DataPoint -> DataPoint
moveAnimal dp = 
    let newX = (fst $ fst dp) + 1
        newY = (snd $ fst dp) + 1
        newDp = ((newX, newY), snd dp)
    in newDp

addAnimal :: DataPoint -> World
addAnimal dp = dp:w

removeDead :: World
removeDead = filter (\x -> (getAlive $ snd x) == True) w

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

animalEvents :: Organism -> Organism
animalEvents = turnAnimal . reproduceAnimal . eatAnimal

dataPointEvents :: DataPoint -> DataPoint
dataPointEvents = moveAnimal



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