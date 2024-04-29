module Models.Shape
  ( Shape
  , area
  ) where

-- Defining Multiple Constructors with Different Fields
data Shape
  = Circle Float -- Circle constructor with a radius
  | Rectangle Float Float -- Rectangle constructor with width and height
  deriving (Show)

-- Pattern Matching
area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h

-- Creating Instances of Typeclasses like Eq, Ord, etc.
instance Eq Shape where
  (Circle r1) == (Circle r2)             = r1 == r2
  (Rectangle w1 h1) == (Rectangle w2 h2) = w1 == w2 && h1 == h2
  _ == _                                 = False

-- Two Circles are equal if their radii are equal.
-- Two Rectangles are equal if their width and height are equal.
-- Otherwise, they are not equal
instance Ord Shape where
  compare (Circle r1) (Circle r2)             = compare r1 r2
  compare (Circle _) _                        = LT
  compare _ (Circle _)                        = GT
  compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1 * h1) (w2 * h2)
-- We compare two Circles based on their radii.
-- We consider any Circle to be less than any Rectangle.
-- We compare two Rectangles based on their areas (width times height).
