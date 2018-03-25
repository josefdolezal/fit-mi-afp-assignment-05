module Data.Shapes where

--------------------------------------------------------------------------------
-- DO NOT CHANGE DATA TYPES DEFINITIONS

newtype Circle = Circle { ciRadius :: Double }
               deriving (Show, Read, Eq)

data Triangle = EquilateralTriangle { etSide :: Double }
              | IsoscelesTriangle { itBase :: Double, itLeg :: Double }
              | ScaleneTriangle { stSideA :: Double, stSideB :: Double, stSideC :: Double }
              deriving (Show, Read, Eq)

data Quadrilateral = Square { sqSide :: Double}
                   | Rectangle { reSideA :: Double, reSideB :: Double }
                   deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

class Validable a where
  valid :: a -> Bool

-- TODO: complete instances for each type to check validity by `valid` function
instance Validable Circle where 
  valid (Circle radius) = radius > 0

instance Validable Triangle where
  valid (EquilateralTriangle side) = side > 0
  valid (IsoscelesTriangle base leg) = leg >= 0 && base >= 0 && 2*leg > base && leg + base > leg
  valid (ScaleneTriangle a b c) = a + b > c && c + a > b && b + c > a

instance Validable Quadrilateral where 
  valid (Square side) = side > 0
  valid (Rectangle a b) = a > 0 && b > 0
-- TODO: create appropriate typeclass for 2D shapes (subclass of Validable)
-- TODO: write instances for the types to compute circumference and area

-- Note: this dummy functions should be placed in typeclass
class Validable a => Computable a where
  area          :: a -> Double
  circumference :: a -> Double

instance Computable Circle where
  area c@(Circle radius) = case (valid c) of
    True -> pi * radius^2
    _    -> 0
  
  circumference c@(Circle radius) = case (valid c) of
    True -> 2 * pi * radius
    _    -> 0

instance Computable Triangle where
  area (EquilateralTriangle side)   = area (ScaleneTriangle side side side)
  area (IsoscelesTriangle base leg) = area (ScaleneTriangle base leg leg)
  area t@(ScaleneTriangle a b c)    = case (valid t) of
    True -> sqrt $ s * (s - a) * (s - b) * (s - c)
              where s = (circumference t) / 2
    _    -> 0

  circumference (EquilateralTriangle side) = circumference (ScaleneTriangle side side side)
  circumference (IsoscelesTriangle base leg) = circumference (ScaleneTriangle base leg leg)
  circumference t@(ScaleneTriangle a b c) = case (valid t) of
    True -> a + b + c
    _    -> 0

instance Computable Quadrilateral where
  area (Square side)   = area (Rectangle side side)
  area q@(Rectangle a b) = case (valid q) of
    True -> a * b
    _    -> 0

  circumference (Square side)   = circumference (Rectangle side side)
  circumference r@(Rectangle a b) = case (valid r) of
    True -> 2 * (a + b)
    _    -> 0

