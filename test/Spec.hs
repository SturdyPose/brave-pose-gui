import Graphics.Primitives
import Test.HUnit
import qualified Linear
import qualified DataStructures.IOSpatialMap as SM


spatialMapTests:: IO ()
spatialMapTests = do
    spatialMap <- SM.createIOSpatialMap (4,4) (40,40)
    let r = Rectangle (Linear.V2 10 10) 20 20

    let ((x1,y1),(x2,y2)) = toBoundary r

    let (floored1, floored2) = ((floor x1, floor y1), (floor x2, floor y2))

    _ <- SM.appendGridRange floored1 floored2 r spatialMap

    _ <- SM.rangeToGridCoords floored1 floored2 spatialMap 
    _ <- SM.lookupCoords (15, 15) spatialMap 
    
    
    return ()

circleTest:: IO ()
circleTest = do
    let r = Circle (Linear.V2 5 5) 10 

    let testCirc = TestList [ 
                TestCase $ assertBool "(0,0) should not be within circle"   $ not $ isPointInsidePolygon r (0,0)
            ,   TestCase $ assertBool "(0,10) should not be within circle"  $ not $ isPointInsidePolygon r (0,10)
            ,   TestCase $ assertBool "(10,10) should not be within circle" $ not $ isPointInsidePolygon r (10,0)
            ,   TestCase $ assertBool "(10,10) should not be within circle" $ not $ isPointInsidePolygon r (10,10)
            ,   TestCase $ assertBool "(-1,5) should not be within circle" $ not $ isPointInsidePolygon r (-1,5)
            ,   TestCase $ assertBool "(11,5) should not be within circle" $ not $ isPointInsidePolygon r (11,5)
            ,   TestCase $ assertBool "(5,-1) should not be within circle" $ not $ isPointInsidePolygon r (5,-1)
            ,   TestCase $ assertBool "(5,11) should not be within circle" $ not $ isPointInsidePolygon r (5,11)
            ,   TestCase $ assertBool "(1,5) should be in circle" $ isPointInsidePolygon r (1, 5)
            ,   TestCase $ assertBool "(9,5) should be in circle" $ isPointInsidePolygon r (9, 5)
            ,   TestCase $ assertBool "(5,1) should be in circle" $ isPointInsidePolygon r (5, 1)
            ,   TestCase $ assertBool "(5,9) should be in circle" $ isPointInsidePolygon r (5, 9)
            ,   TestCase $ assertBool "(6,6) should be in circle" $ isPointInsidePolygon r (6, 6)
            ,   TestCase $ assertBool "(5,5) should be in circle" $ isPointInsidePolygon r (5, 5)
            ,   TestCase $ assertBool "(4,4) should be in circle" $ isPointInsidePolygon r (4, 4)
            ,   TestCase $ assertBool "(4,6) should be in circle" $ isPointInsidePolygon r (4, 6)
            ,   TestCase $ assertBool "(6,5) should be in circle" $ isPointInsidePolygon r (4, 4)
            ]
            
    runTestTT testCirc 
    return ()

testRectanglePoints:: IO ()
testRectanglePoints = do
    let r = Rectangle (Linear.V2 0 0) 10 10
    let testRect = TestList [
                TestCase $ assertBool "(-1,1) should not be within rectangle" $ not $ isPointInsidePolygon r (-1,1)
            ,   TestCase $ assertBool "(11,1) should not be within rectangle" $ not $ isPointInsidePolygon r (11,1)
            ,   TestCase $ assertBool "(1,-1) should not be within rectangle" $ not $ isPointInsidePolygon r (0,-1)
            ,   TestCase $ assertBool "(1,11) should not be within rectangle" $ not $ isPointInsidePolygon r (0,11)
            ,   TestCase $ assertBool "(1,1) should be in rectangle" $ isPointInsidePolygon r (1,1)
            ,   TestCase $ assertBool "(2,2) should be in rectangle" $ isPointInsidePolygon r (2,2)
            ,   TestCase $ assertBool "(3,3) should be in rectangle" $ isPointInsidePolygon r (3,3)
            ,   TestCase $ assertBool "(4,4) should be in rectangle" $ isPointInsidePolygon r (4,4)
            ,   TestCase $ assertBool "(5,5) should be in rectangle" $ isPointInsidePolygon r (5,5)
            ,   TestCase $ assertBool "(6,6) should be in rectangle" $ isPointInsidePolygon r (6,6)
            ,   TestCase $ assertBool "(7,7) should be in rectangle" $ isPointInsidePolygon r (7,7)
            ,   TestCase $ assertBool "(8,8) should be in rectangle" $ isPointInsidePolygon r (8,8)
            ,   TestCase $ assertBool "(9,9) should be in rectangle" $ isPointInsidePolygon r (9,9)
            ,   TestCase $ assertBool "(10,10) should not be within rectangle" $ not $ isPointInsidePolygon r (10,10)
            ]
    
    runTestTT testRect
    return ()


main :: IO ()
main = do 
    testRectanglePoints
    circleTest 
    spatialMapTests
    return ()
