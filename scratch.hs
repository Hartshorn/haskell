import Data.Maybe

toList :: String -> [Integer]
toList input = read ("[" ++ input ++ "]")

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing
                
getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"


divide' :: [Double] -> Double
divide' [] = 1
divide' (x:xs) = x / divide' xs

data Animal = Animal { getAlive :: Bool, getPosition :: (Int, Int) } deriving (Show)

removeDead :: [Animal] -> [Animal]
removeDead [] = []
removeDead a = filter (\x -> getAlive x == True) a














main = do
    putStrLn "Enter a list of numbers: "
    input <- getLine
    let maybeList = getListFromString input in
        case maybeList of
            Just l      -> print (sum l)
            Nothing     -> error "Bad Format. Goodbye"