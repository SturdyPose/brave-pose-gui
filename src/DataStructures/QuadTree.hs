{-# LANGUAGE RankNTypes #-}
-- | A QuadTree can be used to recursively divide up 2D space into quadrants.
--   The smallest division corresponds to an unit `Extent`, so the total depth
--   of the tree will depend on what sized `Extent` you start with.
module DataStructures.QuadTree
        ( QuadTree (..)
        , Quad(..)
        , allQuads
        , emptyTree
        , emptyNode
        , takeQuadOfTree
        , liftToQuad
        , insertByPath
        , insertByCoord
        , lookupNodeByPath
        , lookupByPath
        , lookupByCoord
        , flattenQuadTree
        , flattenQuadTreeWithExtents
        , Extent
        , Coord
        , makeExtent
        , takeExtent
        , squareExtent
        , sizeOfExtent
        , isUnitExtent
        , coordInExtent
        , pointInExtent
        , centerCoordOfExtent
        , cutQuadOfExtent
        , quadOfCoord
        , pathToCoord
        , intersectSegExtent
        , touchesSegExtent)
where

import Data.Maybe (isJust, listToMaybe)

type Point = (Float, Float)

-- | Represents a Quadrant in the 2D plane.
data Quad
        = NW    -- ^ North West
        | NE    -- ^ North East
        | SW    -- ^ South West
        | SE    -- ^ South East
        deriving (Show, Eq, Enum)

-- | A list of all quadrants. Same as @[NW .. SE]@.
allQuads :: [Quad]
allQuads = [NW .. SE]


-- | A rectangular area of the 2D plane.
--   We keep the type abstract to ensure that invalid extents cannot be
--   constructed.
data Extent
        = Extent Int Int Int Int
        deriving (Eq, Show)


-- | An integral coordinate.
type Coord
        = (Int, Int)


-- | Construct an extent.
--      The north value must be > south, and east > west, else `error`.
makeExtent
        :: Int  -- ^ y max (north)
        -> Int  -- ^ y min (south)
        -> Int  -- ^ x max (east)
        -> Int  -- ^ x min (west)
        -> Extent

makeExtent n s e w
        | n >= s, e >= w
        = Extent n s e w

        | otherwise
        = error "Graphics.Gloss.Geometry.Extent.makeExtent: invalid extent"


-- | Take the NSEW components of an extent.
takeExtent :: Extent -> (Int, Int, Int, Int)
takeExtent (Extent n s e w)
        = (n, s, e, w)


-- | A square extent of a given size.
squareExtent :: Int -> Extent
squareExtent i
        = Extent i 0 i 0


-- | Get the width and height of an extent.
sizeOfExtent :: Extent -> (Int, Int)
sizeOfExtent (Extent n s e w)
        = (e - w, n - s)


-- | Check if an extent is a square with a width and height of 1.
isUnitExtent :: Extent -> Bool
isUnitExtent extent
        = sizeOfExtent extent == (1, 1)


-- | Check whether a coordinate lies inside an extent.
coordInExtent :: Extent -> Coord -> Bool
coordInExtent (Extent n s e w) (x, y)
        =  x >= w && x < e
        && y >= s && y < n


-- | Check whether a point lies inside an extent.
pointInExtent :: Extent -> Point -> Bool
pointInExtent (Extent n s e w) (x, y)
 = let  n'      = fromIntegral n
        s'      = fromIntegral s
        e'      = fromIntegral e
        w'      = fromIntegral w

   in   x >= w' && x <= e'
     && y >= s' && y <= n'


-- | Get the coordinate that lies at the center of an extent.
centerCoordOfExtent :: Extent -> (Int, Int)
centerCoordOfExtent (Extent n s e w)
 =      ( w + (e - w) `div` 2
        , s + (n - s) `div` 2)


-- | Cut one quadrant out of an extent.
cutQuadOfExtent :: Quad -> Extent -> Extent
cutQuadOfExtent quad (Extent n s e w)
 = let  hheight = (n - s) `div` 2
        hwidth  = (e - w) `div` 2
   in   case quad of
                NW -> Extent n (s + hheight)  (e - hwidth) w
                NE -> Extent n (s + hheight)  e (w + hwidth)
                SW -> Extent (n - hheight) s  (e - hwidth) w
                SE -> Extent (n - hheight) s  e (w + hwidth)


-- | Get the quadrant that this coordinate lies in, if any.
quadOfCoord :: Extent -> Coord -> Maybe Quad
quadOfCoord extent coord
        = listToMaybe
        $ filter (\q -> coordInExtent (cutQuadOfExtent q extent) coord)
        $ allQuads


-- | Constuct a path to a particular coordinate in an extent.
pathToCoord :: Extent -> Coord -> Maybe [Quad]
pathToCoord extent coord
        | isUnitExtent extent
        = Just []

        | otherwise
        = do    quad    <- quadOfCoord extent coord
                rest    <- pathToCoord (cutQuadOfExtent quad extent) coord
                return  $ quad : rest


-- | If a line segment (P1-P2) intersects the outer edge of an extent then
--   return the intersection point, that is closest to P1, if any.
--   If P1 is inside the extent then `Nothing`.
--
--   @
--                   P2
--                  /
--            ----/-
--            | /  |
--            +    |
--           /------
--         /
--        P1
--   @
--
intersectSegExtent :: Point -> Point -> Extent -> Maybe Point
intersectSegExtent p1@(x1, y1) p2 (Extent n' s' e' w')
        -- starts below extent
        | y1 < s
        , Just pos      <- intersectSegHorzSeg p1 p2 s w e
        = Just pos

        -- starts above extent
        | y1 > n
        , Just pos      <- intersectSegHorzSeg p1 p2 n w e
        = Just pos

        -- starts left of extent
        | x1 < w
        , Just pos      <- intersectSegVertSeg p1 p2 w s n
        = Just pos

        -- starts right of extent
        | x1 > e
        , Just pos      <- intersectSegVertSeg p1 p2 e s n
        = Just pos

        -- must be starting inside extent.
        | otherwise
        = Nothing

        where   n       = fromIntegral n'
                s       = fromIntegral s'
                e       = fromIntegral e'
                w       = fromIntegral w'


-- | Check whether a line segment's endpoints are inside an extent, or if it
--   intersects with the boundary.
touchesSegExtent :: Point -> Point -> Extent -> Bool
touchesSegExtent p1 p2 extent
        =   pointInExtent extent p1
         || pointInExtent extent p2
         || isJust (intersectSegExtent p1 p2 extent)




-- | The quad tree structure.
data QuadTree a
        -- | An empty node.
        = TNil

        -- | A leaf containint some value.
        | TLeaf a

        -- | A node with four children.
        | TNode (QuadTree a) (QuadTree a)       -- NW NE
                (QuadTree a) (QuadTree a)       -- SW SE
        deriving Show


-- | A `TNil` tree.
emptyTree :: QuadTree a
emptyTree = TNil


-- | A node with `TNil`. for all its branches.
emptyNode :: QuadTree a
emptyNode = TNode TNil TNil TNil TNil


-- | Get a quadrant from a node.
--   If the tree does not have an outer node then `Nothing`.
takeQuadOfTree
        :: Quad
        -> QuadTree a
        -> Maybe (QuadTree a)

takeQuadOfTree quad tree
 = case tree of
        TNil            -> Nothing
        TLeaf{}         -> Nothing
        TNode nw ne sw se
         -> case quad of
                NW      -> Just nw
                NE      -> Just ne
                SW      -> Just sw
                SE      -> Just se


-- | Apply a function to a quadrant of a node.
--   If the tree does not have an outer node then return the original tree.
liftToQuad
        :: Quad
        -> (QuadTree a -> QuadTree a)
        -> QuadTree a  -> QuadTree a

liftToQuad quad f tree
 = case tree of
        TNil            -> tree
        TLeaf{}         -> tree
        TNode nw ne sw se
         -> case quad of
                NW      -> TNode (f nw) ne sw se
                NE      -> TNode nw (f ne) sw se
                SW      -> TNode nw ne (f sw) se
                SE      -> TNode nw ne sw (f se)


-- | Insert a value into the tree at the position given by a path.
--   If the path intersects an existing `TLeaf` then return the original tree.
insertByPath :: [Quad] -> a -> QuadTree a -> QuadTree a

insertByPath [] x _
        = TLeaf x

insertByPath (q:qs) x tree
 = case tree of
        TNil    -> liftToQuad q (insertByPath qs x) emptyNode
        TLeaf{} -> tree
        TNode{} -> liftToQuad q (insertByPath qs x) tree


-- | Insert a value into the node containing this coordinate.
--   The node is created at maximum depth, corresponding to an unit `Extent`.
insertByCoord :: Extent -> Coord -> a -> QuadTree a -> Maybe (QuadTree a)
insertByCoord extent coord x tree
 = do   path    <- pathToCoord extent coord
        return  $  insertByPath path x tree


-- | Lookup a node based on a path to it.
lookupNodeByPath
        :: [Quad]
        -> QuadTree a
        -> Maybe (QuadTree a)

lookupNodeByPath [] tree
        = Just tree

lookupNodeByPath (q:qs) tree
 = case tree of
        TNil    -> Nothing
        TLeaf{} -> Nothing
        TNode{}
         -> let Just quad       = takeQuadOfTree q tree
            in  lookupNodeByPath qs quad


-- | Lookup an element based given a path to it.
lookupByPath :: [Quad] -> QuadTree a -> Maybe a
lookupByPath path tree
 = case lookupNodeByPath path tree of
        Just (TLeaf x)  -> Just x
        _               -> Nothing


-- | Lookup a node if a tree given a coordinate which it contains.
lookupByCoord
        :: forall a
        .  Extent       -- ^ Extent that covers the whole tree.
        -> Coord        -- ^ Coordinate of the value of interest.
        -> QuadTree a
        -> Maybe a
lookupByCoord extent coord tree
 = do   path    <- pathToCoord extent coord
        lookupByPath path tree


-- | Flatten a QuadTree into a list of its contained values, with coordinates.
flattenQuadTree
        :: forall a
        .  Extent       -- ^ Extent that covers the whole tree.
        -> QuadTree a
        -> [(Coord, a)]

flattenQuadTree extentInit treeInit
 = flatten' extentInit treeInit
 where  flatten' extent tree
         = case tree of
                TNil    -> []
                TLeaf x
                 -> let (_, s, _, w) = takeExtent extent
                    in  [((w, s), x)]

                TNode{} -> concat $ map (flattenQuad extent tree) allQuads

        flattenQuad extent tree quad
         = let  extent'         = cutQuadOfExtent quad extent
                Just tree'      = takeQuadOfTree quad tree
           in   flatten' extent' tree'


-- | Flatten a QuadTree into a list of its contained values, with extents.
flattenQuadTreeWithExtents
        :: forall a
        .  Extent       -- ^ Extent that covers the whole tree.
        -> QuadTree a
        -> [(Extent, a)]

flattenQuadTreeWithExtents extentInit treeInit
 = flatten' extentInit treeInit
 where  flatten' extent tree
         = case tree of
                TNil    -> []
                TLeaf x
                 -> [(extent, x)]

                TNode{} -> concat $ map (flattenQuad extent tree) allQuads

        flattenQuad extent tree quad
         = let  extent'         = cutQuadOfExtent quad extent
                Just tree'      = takeQuadOfTree quad tree
           in   flatten' extent' tree'


intersectSegHorzSeg
        :: Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Float        -- ^ (y3) y value of horizontal segment.
        -> Float        -- ^ (xa) Leftmost x value of horizontal segment.
        -> Float        -- ^ (xb) Rightmost x value of horizontal segment.
        -> Maybe Point  -- ^ (x3, y3) Intersection point, if any.

intersectSegHorzSeg p1@(x1, y1) p2@(x2, y2) y0 xa xb
        | segClearsBox p1 p2 (xa, y0) (xb, y0)
        = Nothing

        | x0 < xa       = Nothing
        | x0 > xb       = Nothing
        | otherwise     = Just (x0, y0)

        where x0 | (y2 - y1) == 0 = x1
                 | otherwise      = (y0 - y1) * (x2 - x1) / (y2 - y1) + x1

intersectSegVertSeg
        :: Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Float        -- ^ (x3) x value of vertical segment
        -> Float        -- ^ (ya) Lowest y value of vertical segment.
        -> Float        -- ^ (yb) Highest y value of vertical segment.
        -> Maybe Point  -- ^ (x3, y3) Intersection point, if any.

intersectSegVertSeg p1@(x1, y1) p2@(x2, y2) x0 ya yb
        | segClearsBox p1 p2 (x0, ya) (x0, yb)
        = Nothing

        | y0 < ya       = Nothing
        | y0 > yb       = Nothing
        | otherwise     = Just (x0, y0)

        where y0 | (x2 - x1) == 0 = y1
                 | otherwise      = (x0 - x1) * (y2 - y1) / (x2 - x1) + y1



-- | Check if line segment (P1-P2) clears a box (P3-P4) by being well outside it.
segClearsBox
        :: Point        -- ^ P1 First point of segment.
        -> Point        -- ^ P2 Second point of segment.
        -> Point        -- ^ P3 Lower left point of box.
        -> Point        -- ^ P4 Upper right point of box.
        -> Bool

segClearsBox (x1, y1) (x2, y2) (xa, ya) (xb, yb)
        | x1 < xa, x2 < xa      = True
        | x1 > xb, x2 > xb      = True
        | y1 < ya, y2 < ya      = True
        | y1 > yb, y2 > yb      = True
        | otherwise             = False