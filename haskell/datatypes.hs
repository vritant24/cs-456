data Point = 
    Point Float Float 
    deriving(Show)

data Shape = 
        Rectangle Point Point 
    |   Circle Point Float
    deriving(Show)

area :: Shape -> Float
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * (abs (y2 - y1))
area (Circle _ r) = pi * (r^2)