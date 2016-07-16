{-# LANGUAGE ScopedTypeVariables #-}

module City
    (
        genCity
      , cityToStr
    ) where

-- import Data.Random.Normal (normalsIO)
import System.Random (randomRIO, randomIO)
import Control.Monad (join)
import Data.List (intercalate, sortOn, foldl')
import Data.Char (ord, chr)

type Vec2D = (Int, Int)

data Cell =
      Grass
    | Water
    | Building { height :: Int }
    deriving (Eq, Show)

data City = 
    City {   topLeft      :: Vec2D
           , bottomRight  :: Vec2D
           , cellType     :: Cell
           , subCities    :: [City] }
    deriving (Eq, Show)

-- | Return a random point between the
--   rectangle formed by two points
randomPoint
    :: Vec2D    -- ^ One of the points
    -> Vec2D    -- ^ The other point
    -> IO Vec2D -- ^ the random point
randomPoint (x1, y1) (x2, y2) = do
    rx <- randomRIO (x1+1, x2-1)
    ry <- randomRIO (y1+1, y2-1)
    return (rx, ry)

-- | Takes a bunch of probabilities and their respective
--   events and makes a cumulative list of them
--   Ex: [(10, "Eat"), (14, "Play"), (76, "Sleep")] becomes
--       [(10, "Eat"), (24, "Play"), (100, "Sleep")]
cumulativeProbs
    :: (Fractional a, Ord a)
    => [(a, b)] -- ^ Input probabilities
    -> [(a, b)] -- ^ Output probabilities
cumulativeProbs probs = foldl' go [head sortedProbs] (tail sortedProbs)
    where go xs (x,y) = xs ++ [(fst (last xs) + x, y)]
          sortedProbs = sortOn fst probs

-- | Selects an object in a (Probability,Event) list according
--   to the given probability value
selectProb
    :: (Fractional a, Ord a)
    => a
    -> [(a, b)]
    -> b
selectProb prob probEvents =
    go (cumulativeProbs probEvents)
    where go [(_, event)] = event
          go ((eventProb, event):rest)
            | prob<=eventProb = event
            | otherwise = go rest
          go [] = error "selectProb: Empty list received"

-- | Randomly selects an object according to the probabilities
randomSelect
    :: [(Float, b)]
    -> IO b
randomSelect probEvents = do
    (r :: Float) <- randomIO
    let total = sum $ map fst probEvents
    return $ selectProb (r*total) probEvents

buildingMinHt, buildingMaxHt :: Int
buildingMinHt = 1
buildingMaxHt = 18

-- | Return a random building
randomBuilding :: IO Cell
randomBuilding = do
    ht <- randomRIO (buildingMinHt, buildingMaxHt)
    return $ Building ht

-- | Return a Grass cell
grass :: IO Cell
grass = return Grass

-- | Return a Water cell
water :: IO Cell
water = return Water

-- | Probabilities of obtaining cells
cellProbs :: [(Float, IO Cell)]
cellProbs = [(15.0, grass), (10.0, water), (75.0, randomBuilding)]

-- | Return a random cell
randomCell :: IO Cell
randomCell = join $ randomSelect cellProbs

-- | Returns true if the first point is on the
--   boundary formed by the rectangle bounded
--   between the second and third point
pointOnBoundary
    :: Vec2D -- ^ The point to check
    -> Vec2D -- ^ Top-left corner of rectangle
    -> Vec2D -- ^ Bottom-right corner of rectangle
    -> Bool  -- ^ True if point on boundary
pointOnBoundary (cx, cy) (x1, y1) (x2, y2) =
    (cx==x1 || cx==x2) || (cy==y1 || cy==y2)

-- | Returns true if the first point is inside OR ON the
--   boundary formed by the rectangle bounded
--   between the second and third point
pointInsideOrOnBoundary
    :: Vec2D -- ^ The point to check
    -> Vec2D -- ^ Top-left corner of rectangle
    -> Vec2D -- ^ Bottom-right corner of rectangle
    -> Bool  -- ^ True if point inside or on boundary
pointInsideOrOnBoundary (cx, cy) (x1, y1) (x2, y2) =
    (cx>=x1 && cx<=x2) && (cy>=y1 && cy<=y2)

-- | Returns the city which contains the point
cityContainingPoint
    :: [City]
    -> Vec2D
    -> City
cityContainingPoint (c@(City p1 p2 _ _) : rest) p
    | pointInsideOrOnBoundary p p1 p2 = c
    | otherwise = cityContainingPoint rest p
cityContainingPoint [] _ = error "cityContainingPoint: No city found"

-- | Character representation of the Cell
cellToChar
    :: Cell
    -> Char
cellToChar Grass = 'g'
cellToChar Water = 'w'
cellToChar (Building ht)
    | ht<10 = show ht !! 0
    | otherwise = chr $ ord 'A' + ht - 10

-- | Returns a character to print depending on the
--   city's contents at a given point
cityPosToChar
    :: City  -- ^ The city
    -> Vec2D -- ^ The point
    -> Char  -- ^ A character representation
cityPosToChar (City p1 p2 cell subcities) p =
    case pointOnBoundary p p1 p2 of
        True  -> '.'
        False -> if subcities==[]
                    then cellToChar cell
                    else cityPosToChar (cityContainingPoint subcities p) p

-- | Splits a list every n elements
splitEvery 
    :: [a]   -- ^ The list
    -> Int   -- ^ Split every
    -> [[a]] -- ^ result
splitEvery [] _ = []
splitEvery xs n = take n xs : splitEvery (drop n xs) n

-- | Converts a city to its string representation
cityToStr
    :: City   -- ^ The city
    -> String -- ^ City in string form
cityToStr c@(City (x1,y1) (x2,y2) _ _)
    = intercalate "\n" $ splitEvery str (x2-x1+1)
    where str = (cityPosToChar c) <$> [(x,y) | x<-[x1..x2], y<-[y1..y2]]

-- | Generate and return a city
genCity
    :: Vec2D    -- ^ Top-left corner
    -> Vec2D    -- ^ Bottom-right corner
    -> Int      -- ^ Number of subdivisions
    -> IO City  -- ^ Generated city
genCity p1 p2 0 = randomCell >>= \cell -> return $ City p1 p2 cell []
genCity p1@(x1,y1) p2@(x2,y2) subd = do
    r@(rx, ry) <- randomPoint p1 p2
    c1 <- genCity p1 r (subd - 1)
    c2 <- genCity (rx, y1) (x2, ry) (subd - 1)
    c3 <- genCity (x1, ry) (rx, y2) (subd - 1)
    c4 <- genCity r p2 (subd - 1)
    randomCell >>= \cell -> return $ City p1 p2 cell [c1, c2, c3, c4]
