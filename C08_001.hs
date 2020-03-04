module C08_001   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float 
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  


-- We couldn't write a type declaration of Circle -> Float because Circle is not a type, Shape is. 
-- Just like we can't write a function with a type declaration of True -> Int. Becuase True is not a type, Bool is.
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
-- In the rectangle pattern, we just need to use a nested pattern matching to get the fields of the points. 
surface (Rectangle (Point x1 y1) (Point x2 y2) ) = (abs $ x2 - x1) * (abs $ y2 - y1)  

-- Constructors are functions, so we can map them and partially apply them and everything. 
generateCircles rs = map (baseCircle) rs


-- It takes a shape, the amount to move it on the x axis and the amount to move it on the y axis and then returns a new shape that has the same dimensions, only it's located somewhere else.
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  


baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)  


