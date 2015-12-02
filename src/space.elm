module Space 
    ( Space, 
      Space1D, Space2D, Space3D, SpaceColor, 
      f1d, f2d, f3d, fcolor
    ) where

import Color

-- A type can be treated as a "space" if the following operations
-- can be defined for it. Typically this is what we'd call a "vector space".    
-- The key entries are "zero", "add", "scale" and "dot". The rest can
-- be defined in terms of these. However, we have the full suite 'cos
-- we can be more succinct/efficient in some instances.   
--
-- Space is, in essence, what would be called a "type class" in Haskell.
type alias Space a =
    { zero : a                                  -- Constant you can "add" to any value to not change it.
    , add : a -> a -> a                         -- Sum of two vectors in the space gives a vector.
    , sub : a -> a -> a                         -- Difference of two vectors in the space gives a vector.
    , scale : Float -> a -> a                   -- Scale a vector by a given scalar factor. You can simulate
                                                --    "sub" using add and scale, but many of these functions
                                                --    are redundant for convenience and efficiency's sake.
    , madd : Float -> a -> a -> a               -- madd f v1 v2 = f * v1 + v2. This is a common operation.
    , lsum : Float -> a -> Float -> a -> a      -- "linear sum" - lsum f1 v1 f2 v2 = f1 * v1 + f2 * v2
    , lerp : Float -> a -> a -> a               -- lerp f v1 v2 = lsum (1-f) v1 f v2
    , dot : a -> a -> Float                     -- Scalar product of vectors
    , dir : a -> a                              -- The unit direction vector. For a scalar Float type, this
                                                --    gives the sign. dir v = scale (1/abs v) v
    , abs : a -> Float                          -- The magnitude of the vector. abs v = sqrt (norm v)
    , norm : a -> Float                         -- The magnitude squared of the vector. norm v = dot v v
    , dist : a -> a -> Float                    -- dist v1 v2 = abs (sub v1 v2)
    }

type alias Point1D = Float
type alias Point2D = (Float, Float)
type alias Point3D = (Float, Float, Float)
type alias PointColor = Color.Color

type alias Space1D = Space Point1D
type alias Space2D = Space Point2D
type alias Space3D = Space Point3D
type alias SpaceColor = Space PointColor

f1d : Space1D
f1d =
    { zero = 0.0
    , add = (+)
    , sub = (-)
    , scale = (*)
    , madd = \f x1 x2 -> f * x1 + x2
    , lsum = \f1 x1 f2 x2 -> f1 * x1 + f2 * x2
    , lerp = \f x1 x2 -> x1 + f * (x2 - x1)
    , dot = (*)
    , dir = \a -> if a > 0.0 then 1.0 else if a < 0.0 then -1.0 else 0.0
    , abs = abs
    , norm = \x -> x * x
    , dist = \p1 p2 -> f1d.abs (f1d.sub p1 p2)
    }

f2d : Space2D
f2d =
    { zero = (0.0, 0.0)
    , add = \(x1,y1) (x2,y2) -> (x1+x2,y1+y2)
    , sub = \(x1,y1) (x2,y2) -> (x1-x2,y1-y2)
    , scale = \f (x,y) -> (f * x, f * y)
    , madd = \f (x1,y1) (x2,y2) -> (f * x1 + x2, f * y1 + y2)
    , lsum = \f1 (x1,y1) f2 (x2,y2) -> (f1 * x1 + f2 * x2, f1 * y1 + f2 * y2)
    , lerp = \f p1 p2 -> f2d.lsum (1.0-f) p1 f p2
    , dot = \(x1,y1) (x2,y2) -> x1 * x2 + y1 * y2
    , dir = \p -> f2d.scale (1.0 / f2d.abs p) p
    , abs = \p -> sqrt (f2d.norm p)
    , norm = \(x,y) -> x * x + y * y
    , dist = \p1 p2 -> f2d.abs (f2d.sub p1 p2)
    }

f3d : Space3D
f3d =
    { zero = (0.0, 0.0, 0.0)
    , add = \(x1,y1,z1) (x2,y2,z2) -> (x1+x2,y1+y2,z1+z2)
    , sub = \(x1,y1,z1) (x2,y2,z2) -> (x1-x2,y1-y2,z1-z2)
    , scale = \f (x,y,z) -> (f * x, f * y, f * z)
    , madd = \f (x1,y1,z1) (x2,y2,z2) -> (f * x1 + x2, f * y1 + y2, f * z1 + z2)
    , lsum = \f1 (x1,y1,z1) f2 (x2,y2,z2) -> (f1 * x1 + f2 * x2, f1 * y1 + f2 * y2, f1 * z1 + f2 * z2)
    , lerp = \f p1 p2 -> f3d.lsum (1.0-f) p1 f p2
    , dot = \(x1,y1,z1) (x2,y2,z2) -> x1 * x2 + y1 * y2 + z1 * z2
    , dir = \p -> f3d.scale (1.0 / f3d.abs p) p
    , abs = \p -> sqrt (f3d.norm p)
    , norm = \(x,y,z) -> x * x + y * y + z * z
    , dist = \p1 p2 -> f3d.abs (f3d.sub p1 p2)
    }

fcolor : SpaceColor
fcolor =
    { zero = Color.black
    , add = \c1 c2 -> let (rgb1,rgb2) = (Color.toRgb c1, Color.toRgb c2)
                          alpha = rgb1.alpha + rgb2.alpha
                      in Color.rgba
                        (floor ((toFloat rgb1.red * rgb1.alpha + toFloat rgb2.red * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.green * rgb1.alpha + toFloat rgb2.green * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.blue * rgb1.alpha + toFloat rgb2.blue * rgb2.alpha) / alpha))
                        (alpha)
    , sub = \c1 c2 -> let (rgb1,rgb2) = (Color.toRgb c1, Color.toRgb c2)
                          alpha = rgb1.alpha + rgb2.alpha
                      in Color.rgba
                        (floor ((toFloat rgb1.red * rgb1.alpha - toFloat rgb2.red * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.green * rgb1.alpha - toFloat rgb2.green * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.blue * rgb1.alpha - toFloat rgb2.blue * rgb2.alpha) / alpha))
                        (alpha)
    , scale = \f c -> let rgba = Color.toRgb c
                      in Color.rgba rgba.red rgba.green rgba.blue (f * rgba.alpha)
    , madd = \f c1 c2 -> fcolor.add c2 (fcolor.scale f c1)
    , lsum = \f1 c1 f2 c2 -> fcolor.add (fcolor.scale f1 c1) (fcolor.scale f2 c2)
    , lerp = \f p1 p2 -> fcolor.lsum (1.0-f) p1 f p2
    , dot = \c1 c2 -> let (rgb1,rgb2) = (Color.toRgb c1, Color.toRgb c2)
                      in sqrt (toFloat (rgb1.red * rgb2.red + rgb1.green * rgb2.green + rgb1.blue * rgb2.blue))
    , dir = \p -> fcolor.scale (1.0 / fcolor.abs p) p
    , abs = \p -> sqrt (fcolor.norm p)
    , norm = \c -> let rgb = Color.toRgb c
                   in toFloat (rgb.red * rgb.red + rgb.green * rgb.green + rgb.blue * rgb.blue)
    , dist = \p1 p2 -> fcolor.abs (fcolor.sub p1 p2)
    }

