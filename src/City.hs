{-# LANGUAGE ScopedTypeVariables #-}

module City
    (
        genCity
      , cityToStr
    ) where

-- import Data.Random.Normal (normalsIO)
import System.Random (randomRIO, randomIO)
import Data.List (intercalate)

type Vec2D = (Int, Int)

data Cell =
      Grass
    | Building Int
    deriving (Eq, Show)

data City = 
    City {   topLeft   :: Vec2D
           , topRight  :: Vec2D
           , cellType  :: Cell
           , subCities :: [City] }
    deriving (Eq, Show)

-- | Return a random point between two points
randomPoint
    :: Vec2D    -- ^ One of the points
    -> Vec2D    -- ^ The other point
    -> IO Vec2D -- ^ the random point
randomPoint (x1, y1) (x2, y2) = do
    rx <- randomRIO (x1+1, x2-1)
    ry <- randomRIO (y1+1, y2-1)
    return (rx, ry)

buildingMinHt, buildingMaxHt :: Int
buildingMinHt = 1
buildingMaxHt = 6

-- | Return a random building
randomBuilding :: IO Cell
randomBuilding = do
    ht <- randomRIO (buildingMinHt, buildingMaxHt)
    return $ Building ht

-- | Return a random cell
randomCell :: IO Cell
randomCell = do
    (r :: Float) <- randomIO
    if r<0.1 -- 10% chance of being grass
        then return Grass
        else randomBuilding

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
cityContainingPoint [c] p = c
cityContainingPoint (c@(City p1 p2 _ _) : rest) p
    | pointInsideOrOnBoundary p p1 p2 = c
    | otherwise = cityContainingPoint rest p

-- | Character representation of the Cell
cellToChar
    :: Cell
    -> Char
cellToChar Grass = 'G'
cellToChar (Building ht) = show ht !! 0

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
