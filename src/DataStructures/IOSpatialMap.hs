{-# LANGUAGE TemplateHaskell #-}
module DataStructures.IOSpatialMap where

import qualified Data.Vector.Mutable as MV

import Data.Maybe (fromMaybe)
import GHC.Float (int2Float)
import Control.Monad (void)
import Data.Tuple (swap)
import Data.Ord (clamp)
import Data.Foldable (foldrM)
import Data.IORef
import Data.Bits (Bits(shiftL))
import Data.List (uncons)

data SpatialData a = SpatialData {
    _spatialData::  !a
  , _spatialDataX:: !Int
  , _spatialDataY:: !Int
} deriving (Show, Eq)

data LinearCache a = LinearCache {
    _linearCacheData         :: !(MV.IOVector (Maybe (SpatialData a)))
  , _linearCacheEmptyIndices :: !(IORef [Int])
  , _linearCacheSize         :: !(IORef Int) -- size is the size of vector 
  , _linearCacheLength       :: !(IORef Int) -- length is number of Just elements in vector
}

createLinearCache:: IO(LinearCache a)
createLinearCache = do
  initCache <- MV.replicate 2 Nothing
  initIndices <- newIORef [0,1]
  initCacheSize <- newIORef 2
  initLength <- newIORef 0
  return LinearCache {
      _linearCacheData = initCache
    , _linearCacheEmptyIndices = initIndices
    , _linearCacheSize = initCacheSize 
    , _linearCacheLength = initLength
  }


insert:: LinearCache a -> SpatialData a -> IO ()
insert cache@LinearCache{ _linearCacheData = dat, _linearCacheEmptyIndices = refindices, _linearCacheSize = refsize, _linearCacheLength = reflength} a = do
  indices <- readIORef refindices
  case uncons indices of
    Nothing -> do
      size <- readIORef refsize
      let newSize = size `shiftL` 1
      copyCache <- MV.replicate size Nothing
      MV.imapM_ (MV.unsafeWrite copyCache ) dat

      writeIORef refsize newSize 
      writeIORef refindices [size..newSize - 1] 
      return ()
    Just (i, rest) -> do 
      MV.unsafeWrite dat i (Just a)
      modifyIORef reflength (+1) 
      writeIORef refindices rest 

removeAt:: LinearCache a -> Int -> IO ()
removeAt cache@LinearCache{ _linearCacheData = dat, _linearCacheEmptyIndices = indices, _linearCacheLength = length} i = do
  MV.write dat i Nothing
  return cache {
      _linearCacheEmptyIndices = i:indices
    , _linearCacheLength = length - 1 
  }

-- remove:: LinearCache a -> SpatialData a -> IO ()
-- remove cache@LinearCache{ _linearCacheData = dat, _linearCacheEmptyIndices = indices, _linearCacheLength = length} val = do
--   MV.imapM_ (\i x -> if x == val )

instance Functor SpatialData where
  fmap f a@SpatialData{_spatialData = dat} = a{_spatialData = f dat} 

data IOSpatialMap a = IOSpatialMap
  {
      _grid        :: !(MV.IOVector [SpatialData a])
    , _rows        :: !Int
    , _columns     :: !Int
    , _width       :: !Int
    , _height      :: !Int
  }

printMap:: Show a => IOSpatialMap a -> IO String
printMap IOSpatialMap{ _width = width, _height = height, _columns= columns, _rows = rows, _grid = grid} = do
  val <- MV.foldr (:) [[]] grid
  return $ show val


-- instance Functor IOSpatialMap where
--   fmap f m@(IOSpatialMap {_grid = g}) = m{_grid = V.foldr (\x acc -> fmap f x `V.cons` acc) V.empty g}


-- | Create spatial hashmap with empty grid list
createIOSpatialMap:: (Int,Int) -> (Int,Int) -> IO (IOSpatialMap a)
createIOSpatialMap (rows,columns) (width,height) = do
  initGrid <- MV.replicate (rows * columns) []
  return IOSpatialMap { 
      _grid = initGrid
    , _rows = rows
    , _columns = columns
    , _width = width
    , _height = height
  }


readDataFromGrid:: (Int,Int) -> IOSpatialMap a -> IO [SpatialData a]
readDataFromGrid (x, y) IOSpatialMap{_grid = grid, _rows = rows, _columns = cols} = do
  m_val <- grid `MV.readMaybe` (y * rows + x)
  return $ fromMaybe [] m_val

appendToGrid:: (Int,Int) -> a -> IOSpatialMap a -> IO (IOSpatialMap a)
-- insertGridCoord (x,y) a b@IOSpatialMap{ _grid = grid, _rows = rows} = let val = a : lookupAsGrid (x,y) b in let updatedGrid = V.modify (\v -> MV.write v (x + y*rows) val) grid in b{_grid= updatedGrid }
appendToGrid (x,y) dat b@IOSpatialMap{ _grid = grid, _rows = rows, _columns = cols} = do
  -- print $ MV.length grid
  let spDat = SpatialData{_spatialData = dat, _spatialDataX = x, _spatialDataY = y} 
  val <- (spDat :) <$> readDataFromGrid (x,y) b
  MV.write grid (y * rows + x) val
  return b


lookupCoords:: (Int, Int) -> IOSpatialMap a -> IO [SpatialData a]
lookupCoords point grid = readDataFromGrid (swap $ pointToCoord point grid) grid

pointToCoord:: (Int, Int) -> IOSpatialMap a -> (Int, Int)
pointToCoord (x,y) grid = (clamp (0, rows - 1) rowCoord, clamp (0,columns - 1) columnCoord)
  where 
    IOSpatialMap{ _width = width, _height = height, _columns= columns, _rows = rows } = grid
    columnCoord = x `div` (width `div` columns)
    rowCoord = y `div` (height `div` rows)



-- Transform rectangle on the grid system to grid coordinates
rangeToGridCoords:: (Int, Int) -> (Int, Int) -> IOSpatialMap a -> IO [(Int,Int)]
rangeToGridCoords point1@(x1, y1) point2@(x2,y2) a@IOSpatialMap{ _width = width, _height = height, _columns= columns, _rows = rows }
  | (x1 < 0 && x2 < 0) || (y1 < 0 && y2 < 0) || (x1 > width && x2 > width) || (y1 > height && y2 > height) = return []
  | y2 < y1 && x2 < x1 = rangeToGridCoords (x2, y2) (x1,y1) a
  | otherwise = do
      return grid
  where
    startPoint@(gridStartX, gridStartY) = pointToCoord point1 a 
    endPoint@(gridEndX, gridEndY) = pointToCoord point2 a 
    grid = [(y,x)| y <- [gridStartY..gridEndY], x <- [gridStartX..gridEndX]]
    -- grid = [(spanOfColumns, spanOfRows)]

-- | Search within a rectangle 
-- | arguments are corners of the rectangle
searchWithinRange:: (Int, Int) -> (Int, Int) -> IOSpatialMap a -> IO [SpatialData a]
searchWithinRange corner1 corner2 hMap = do
  rangeValues <- rangeToGridCoords corner1 corner2 hMap
  concat <$> traverse (`readDataFromGrid` hMap) rangeValues

-- | Search within a rectangle 
-- | arguments are corners of the rectangle
appendGridRange:: (Int, Int) -> (Int, Int) -> a -> IOSpatialMap a -> IO (IOSpatialMap a)
appendGridRange corner1 corner2 a hMap = do
  rangeValues <- rangeToGridCoords corner1 corner2 hMap
  mapM_ (\val -> void $ appendToGrid val a hMap) rangeValues
  return hMap

-- testMap:: IOSpatialMap String
-- testMap = m6
--   where
--     m = createIOSpatialMap (2,3) (100,100)
--     m1 = insertGridCoord (0,0) "a" m
--     m2 = insertGridCoord (0,1) "b" m1
--     m3 = insertGridCoord (1,0) "d" m2
--     m4 = insertGridCoord (1,1) "e" m3
--     m5 = insertGridCoord (1,2) "f" m4
--     m6 = insertGridCoord (1,2) "f" m5

imapM:: (Foldable t, Monad m) => (Int -> a -> m b) -> t a -> m [b]
imapM callback foldableThing = fst <$> foldrM (\x (acc, i) -> callback i x >>= \val -> return (val:acc,i+1)) ([], 0) foldableThing

forEach:: IOSpatialMap a -> (Int -> Int -> SpatialData a -> IO b) -> IO [b]
forEach IOSpatialMap{_grid = grid} callback =
  concat . fst <$> MV.foldrM 
    (\list_of_a (acc, indexColumn) -> imapM (callback indexColumn) list_of_a >>= \val -> return (val:acc, indexColumn + 1)) 
    ([], 0)
    grid
