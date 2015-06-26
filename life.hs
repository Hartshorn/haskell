type Pos    = (Int, Int)
type Board  = [Pos]

width   :: Int
width   = 100
height  :: Int
height  = 80

flat :: Board
flat = [(2,10),(3,10),(4,10),(5,10),(6,10),(7,10),(8,10),(9,10),
        (12,10),(13,10),(14,10),(15,10),(16,10),
        (20,10),(21,10),(22,10),
        (29,10),(30,10),(31,10),(32,10),(33,10),(34,10),(35,10),
        (37,10),(38,10),(39,10),(40,10),(41,10)]
        
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


shiftright :: Int -> Board -> Board
shiftright n b = zip [x | x <- map (+n) $ map (fst) b] (map (snd) b)

shiftdown :: Int -> Board -> Board
shiftdown n b = zip (map (fst) b) [y | y <- map (+n) $ map (snd) b]

shift :: Int -> Int -> Board -> Board
shift x y = shiftright x . shiftdown y

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as


showcells :: Board -> IO ()
showcells b = seqn [writeat p "*" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not . isAlive b

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x, y-1),
                          (x+1, y-1), (x-1, y),
                          (x+1, y)  , (x-1, y+1),
                          (x, y+1 ) , (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, 
              ((y-1) `mod` height + 1))

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births b = [p | p <- rmdups . concat . map neighbs $ b,
                isEmpty b p,
                liveneighbs b p == 3]
                
rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 90000
            life . nextgen $ b