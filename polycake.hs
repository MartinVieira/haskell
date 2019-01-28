import System.IO
import Data.List
import Text.Printf

type Point = (Double, Double)
type Segment = (Point, Point)
type Length = Double

data Polygon = Polygon { vertices :: Int
                       , yline :: Double
                       , points :: [Point]
                       } deriving (Show)

main = do  
    let list = []
    handle <- openFile "polycake.in" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = fileToList singlewords
        n = list!!0
        list' = drop 1 list
        polygons = polyList n list'  
        output = outList polygons 
    print (output)
    hClose handle 

outList :: [Polygon] -> [String]
outList polygons = map (\poly -> lengthsToString . fromJust $
                                 calcPerim (yline poly) (points poly)) polygons

show3Decimals :: Double -> String
show3Decimals x = printf "%.3f" x

lengthsToString :: (Double, Double) -> String
lengthsToString (min, max) = show3Decimals min ++ " " ++ show3Decimals max

maxLength :: (Length, Length) -> Length
maxLength (a, b) =
    if a > b
        then a
        else b

minLength :: (Length, Length) -> Length
minLength (a, b) =
    if a < b
        then a
        else b

fromJust :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

fileToList :: [String] -> [Int]
fileToList = map read

polyList :: Int -> [Int] -> [Polygon]
polyList n [] = []
polyList _ [x] = error "Too few points remaining"
polyList n (v:y:list') =
    let pointList = take (2*v) list' -- Note: list' may not *have* 2*v points
        points = getPoints pointList
        list'' = drop (2*v) list'
        poly = Polygon { vertices = v, yline = fromIntegral y, points = points}
        nextPoly = polyList (n-1) list''
    in (poly:nextPoly)

getPoints :: [Int] -> [Point]
getPoints [] = []
getPoints (k:v:t) = (fromIntegral k, fromIntegral v) : getPoints t

segmentCompare :: Double -> Segment -> Ordering
segmentCompare y (p,q) =                                                                          
    case () of                                                                                        
        _ | all (`isUnder` y) [p,q] -> LT
        _ | all (`isOver` y)  [p,q] -> GT
        _ -> EQ

partition3 :: (Segment -> Ordering) -> [Segment] -> ([Segment], [Segment], [Segment])
partition3 f = p' ([], [], [])
  where
    p' (lt, eq, gt) (x:xs) =
        case f x of
            LT -> p' (x:lt, eq, gt) xs
            EQ -> p' (lt, x:eq, gt) xs
            GT -> p' (lt, eq, x:gt) xs
    p' result [] = result

divvy :: Double -> Segment -> (Segment, Segment, Point)
divvy y (start, end) =
    if start `isUnder` y
    then ((start, middle), (middle, end), middle)
    else ((middle, end), (start, middle), middle)
  where
    middle = intersectPoint y (start, end)

splitPolygon :: Double -> [Point] -> Maybe ([Segment], [Segment])
splitPolygon y list = do
    let (under, crossing, over) = partition3 (segmentCompare y) pairs
    case crossing of
        -- No lines cross. Simple.
        [] -> return (under, over)
        -- Two segments cross. Divide them up.
        [(p1,p2),(q1,q2)] ->
            let (u1, o1, mid1) = divvy y (p1,p2)
                (u2, o2, mid2) = divvy y (q1, q2)
                split = (mid1, mid2) :: Segment
            in return (split:u1:u2:under, split:o1:o2:over)
        -- More segments cross. Algorithm doesn't work.
        rest -> fail "Can't split polygons concave at y"
  where
    pairs = zip list (drop 1 $ cycle list) :: [Segment]

calcPerim :: Double -> [Point] -> Maybe (Length, Length)
calcPerim y list = do
    (under, over) <- (splitPolygon y list :: Maybe ([Segment], [Segment]))
    return (sumSegments under, sumSegments over)

distance :: Segment -> Length
distance ((ax, ay), (bx, by)) = sqrt $ (bx-ax)^2 + (by-ay)^2

intersectPoint :: Double -> Segment -> Point
intersectPoint y ((px, py), (qx, qy)) =
    let t = (y-py)/(qy-py)
        x = px + t * (qx - px)
    in
        (x,y)

sumSegments :: [Segment] -> Length
sumSegments = sum . map distance

isUnder :: Point -> Double -> Bool
isUnder (_, py) y = py < y
isOver (_, py) y = py > y