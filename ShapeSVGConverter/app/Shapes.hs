module Shapes(
  Shape(Circle, Square, Empty), Point, Vector(Vector), Transform(Stroke,StrokeWidth,Shading,Scale, Rotate, Identity, Translate, Compose),
  Drawing, Picture, Colour(Red, Black), Matrix(Matrix),
  point, getX, getY,
  empty, circle, square,
  stroke,strokewidth,shading,identity, translate, rotate, scale, (<+>),
  inside)  where


-- Utilities

data Vector = Vector Double Double
              deriving (Show,Read)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving (Show,Read)

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty
           | Circle
           | Square
             deriving (Show,Read)

type Empty = Shape
type Circle = Shape
type Square = Shape
empty = Empty
circle = Circle
square = Square

-- Transformations
-- Style based transformations (Shading, Stroke, StrokeWidth) were added for CS4012 assignment
-- Rotate was implemented differently as Rotate2 was to find new position of a point and was transferable to my SVG shapeConverter
-- Rotate take 3 Ints. The first two repesenting the point to rotate around and the second the degree of rotation
-- Rotate x y angle
data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Int Int Int
           | Rotate2 Matrix
           | Shading Colour
           | Stroke Colour
           | StrokeWidth Int
             deriving (Show,Read)

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
rotate2 angle = Rotate2 $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1
shading = Shading
strokewidth = StrokeWidth
stroke = Stroke

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate2 m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings
data Picture = Drawing
                deriving (Show,Read)
drawing = Drawing
type Drawing = [(Transform,Shape)]


data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Enum, Read)

-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1


distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)
