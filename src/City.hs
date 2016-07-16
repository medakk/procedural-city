{-# LANGUAGE ScopedTypeVariables #-}

module City
    (   genCity
      , cityToStr
      , cityMeshToStr
    ) where

import Data.Random.Normal (normalIO)
import System.Random (randomRIO, randomIO)
import Control.Monad (join)
import Data.List (intercalate, sortOn)
import Data.Char (ord, chr)

type Vec2D = (Int, Int)

data Cell =
      NoCell
    | Grass
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

-- | Selects an object in a (Probability,Event) list according
--   to the given probability value
selectProb
    :: (Fractional a, Ord a)
    => a
    -> [(a, b)]
    -> b
selectProb prob probEvents =
    go prob (sortOn fst probEvents)
    where go p [(_, event)] = event
          go p ((eventProb, event):rest)
            | p<=eventProb = event
            | otherwise = go (p-eventProb) rest
          go _ [] = error "selectProb: Empty list received"

-- | Randomly selects an object according to the probabilities
randomSelect
    :: [(Float, b)]
    -> IO b
randomSelect probEvents = do
    (r :: Float) <- randomIO
    let total = sum $ map fst probEvents
    return $ selectProb (r*total) probEvents

-- | Cap a value within a range
cap
    :: (Num a, Ord a)
    => a -- ^ Value to be capped
    -> a -- ^ Lower bound
    -> a -- ^ Upper bound
    -> a -- ^ Capped value
cap val lb ub
    | val<=lb = lb
    | val>=ub = ub
    | otherwise = val

buildingMinHt, buildingMaxHt :: Int
buildingMinHt = 1
buildingMaxHt = 18

-- | Return a random building
randomBuilding :: IO Cell
randomBuilding = do
    (r :: Float) <- normalIO
    let mean = fromIntegral $ (buildingMinHt + buildingMaxHt) `div` 2
        ht = cap (round $ r*(10/3) + mean) buildingMinHt buildingMaxHt
    return $ Building ht

-- | Return a Grass cell
grass :: IO Cell
grass = return Grass

-- | Return a Water cell
water :: IO Cell
water = return Water

-- | Probabilities of obtaining cells
cellProbs :: [(Float, IO Cell)]
cellProbs = [(15.0, grass), (5.0, water), (80.0, randomBuilding)]

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
cellToChar NoCell = error "cellToChar: Trying to convert NoCell to char"

cityPosToChar
    :: City  -- ^ The city
    -> Vec2D -- ^ The point
    -> Char  -- ^ A character representation
cityPosToChar (City p1 p2 cell []) p = cellToChar  cell
cityPosToChar (City p1 p2 cell subcities) p =
    cityPosToChar (cityContainingPoint subcities p) p

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

-- Converts a Vec2D to a string in the form "x y"
vec2DToStr
    :: Vec2D
    -> String
vec2DToStr (x, y) = show x ++ " " ++ show y

-- | Converts a city to its meshed string representation
cityMeshToStr
    :: City   -- ^ The city
    -> String -- ^ String mesh
cityMeshToStr (City p1 p2 cell [])
    = vec2DToStr p1 ++ " " ++ vec2DToStr p2 ++ " " ++ (cellToChar cell : "")
cityMeshToStr (City _ _ _ subcities)
    = intercalate "\n" $ fmap cityMeshToStr subcities

-- | Generate and return a city
genCity
    :: Vec2D    -- ^ Top-left corner
    -> Vec2D    -- ^ Bottom-right corner
    -> Int      -- ^ Number of subdivisions
    -> IO City  -- ^ Generated city
genCity p1 p2 0 = (\cell -> City p1 p2 cell []) <$> randomCell
genCity p1@(x1,y1) p2@(x2,y2) subd = do
    r@(rx, ry) <- randomPoint p1 p2
    c1 <- genCity p1 r (subd - 1)
    c2 <- genCity (rx, y1) (x2, ry) (subd - 1)
    c3 <- genCity (x1, ry) (rx, y2) (subd - 1)
    c4 <- genCity r p2 (subd - 1)
    return $ City p1 p2 NoCell [c1, c2, c3, c4]
