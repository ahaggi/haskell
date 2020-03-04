module C08_002   
( Point(..)  
, Shape -- We do not export any constructors for Shape. That way, someone importing our module could only make shapes by using the baseCircle and baseRect. 
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  

data Point = Point {x::Float , y::Float} deriving (Show)  
data Shape = Circle {pnt::Point ,  r::Float} | Rectangle {upperRightPnt::Point , lowerLeftPnt::Point} deriving (Show)  


-- We couldn't write a type declaration of Circle -> Float because Circle is not a type, Shape is. 
-- Just like we can't write a function with a type declaration of True -> Int. Becuase True is not a type, Bool is.
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
-- In the rectangle pattern, we just need to use a nested pattern matching to get the fields of the points. 
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)  

-- ghci>  surface $ Circle {pnt = (Point {x=0 , y=0}) , r = 1}
-- 3.1415927
-- OR
-- ghci>  surface $ Circle (Point 0 0) 1
-- 3.1415927


-- Constructors are functions, so we can map them and partially apply them and everything. 
generateCircles rs = map (baseCircle) rs

type DeltaX = Float
type DeltaY = Float

-- It takes a shape, the amount to move it on the x axis and the amount to move it on the y axis and then returns a new shape that has the same dimensions, only it's located somewhere else.
nudge :: Shape -> DeltaX -> DeltaY -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  

-- ghci>  nudge (baseRect 40 100) 60 23
-- Rectangle {upperRightPnt = Point {x = 60.0, y = 23.0}, lowerLeftPnt = Point {x = 100.0, y = 123.0}}


baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  

baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)  


