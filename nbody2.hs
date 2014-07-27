module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Planets = Planets { positions :: !(V.Vector (U.Vector Double)), velocities :: !(V.Vector (U.Vector Double)), masses :: !(V.Vector Double)} deriving Show

--------------------------------------------------------------------------------
-- Physics style vectors using Data.Vector.Unboxed
--

udProduct :: U.Vector Double -> Double -> U.Vector Double
udProduct u d = U.map (*d) u

duProduct :: Double -> U.Vector Double -> U.Vector Double
duProduct d u = U.map (*d) u

uuSum :: U.Vector Double -> U.Vector Double -> U.Vector Double
uuSum u1 u2 = U.zipWith (+) u1 u2

uuDifference :: U.Vector Double -> U.Vector Double -> U.Vector Double
uuDifference u1 u2 = U.zipWith (-) u1 u2

uMagnitude :: U.Vector Double ->  Double
uMagnitude u = sqrt (U.sum (U.zipWith (*) u u))

uZero3 :: U.Vector Double
uZero3 = U.replicate 3 (0.0::Double)


--------------------------------------------------------------------------------
-- Helper for Data.Vector
--

-- d, [ v0 v1 v2 v3 ] -> [ d * v0, d * v1, d * v2, d * v3 ]
dvProduct :: Double -> V.Vector Double -> V.Vector Double
dvProduct d v = V.map (* d) v

-- [1,2,3,4] -> [ [1,2,3,4], [2,3,4], [3,4], [4] ]
subVecsInOrder v = V.iterateN (V.length v) V.unsafeTail v

--------------------------------------------------------------------------------
-- Matrices (usually of physics style vectors) using nested Data.Vector
--

-- [ [ f 0 0,              ]
--   [ f 1 0, f 1 1,       ]
--   [ f 2 0, f 2 1, f 2 2 ] ]
makeLLTD f n = let makerow row = V.generate (1+row) (f row)
               in V.generate n makerow

-- [ [ f 0 1, f 0 2, f 0 3 ],
--   [        f 1 2, f 1 3 ],
--   [               f 2 3 ] ]
makeURT f n = let g i j = f i (j+i+1)
                  makerow i = V.generate (n-i) (g i)
              in V.generate n makerow 

-- [ [ g00 g01 g02],  [m0, m1, m2] -> [ [ f g00 m0, f g01 m1, f g02 m2 ],
--   [ g10 g11 g12],                    [ f g10 m0, f g11 m1, f g12 m2 ],
--   [ g20 g21 g22] ],                  [ f g20 m0, f g21 m1, f g22 m2 ] ]
vvZipWithV f vv v = V.map (V.zipWith (flip f) v) vv

-- [ [ g00 g01 g02],  [m0, m1, m2] -> [ [ f g00 m0 `uuSum` f g01 m1 `uuSum` f g02 m2 ],
--   [ g10 g11 g12],                    [ f g10 m0 `uuSum` f g11 m1 `uuSum` f g12 m2 ],
--   [ g20 g21 g22] ],                  [ f g20 m0 `uuSum` f g21 m1 `uuSum` f g22 m2 ] ]
vvDotMapV f vv v = V.map (V.foldl uuSum uZero3) $ vvZipWithV f vv v

-- [ [ g00 g01 g02 ],   [ m0, m1, m2 ] -> [ f g00 m0 + f g01 m1 + f g02 m2,
--   [     g11 g12 ],                                  f g11 m1 + f g12 m2,
--   [         g22 ] ],                                           f g22 m2 ]
vvURTDotMapV f vv v = let vsvs = subVecsInOrder v
                      in V.map (V.foldl uuSum uZero3) $ V.zipWith (V.zipWith f) vv vsvs

--------------------------------------------------------------------------------
-- Physics

-- The positional part of the force on i due to j.
-- \delta\vec{x} / |\delta\vec{x}|^3 
grav planets i j = let pos  = (positions planets)
                       dx   = (pos V.! j) `uuDifference` (pos V.! i)
                       idxc = 1.0 / (uMagnitude dx) ^ 3
                   in dx `udProduct` idxc

-- The upper right triangle of the forces between planets.
-- Matrix of vectors.
gravURT planets = makeURT (grav planets) ((V.length (masses planets))-1)

-- The lower left triangle
-- From the upper right using the fact that the matrix is antisymmetric
gravLLT urt = let f i j = (urt V.! j V.! (i-j)) `udProduct` (-1.0)
              in makeLLTD f (V.length urt)

advance 0 _  planets = planets
advance n dt planets = let pmasses                      = (masses planets)
                           massesDt                     = dvProduct dt pmasses
                           gURT                         = gravURT planets
                           gLLT                         = gravLLT gURT
                           gmURT                        = vvURTDotMapV udProduct gURT (V.unsafeTail massesDt)
                           gmLLT                        = vvDotMapV udProduct gLLT massesDt
                           nPlanets                     = V.length massesDt
                           lastPlanet                   = nPlanets - 1
                           deltaVel i | i == 0          = gmURT V.! 0
                                      | i == lastPlanet = gmLLT V.! (lastPlanet-1)
                                      | otherwise       = (gmURT V.! i) `uuSum` (gmLLT V.! (i-1))
                           deltaVels                    = V.generate nPlanets deltaVel
                           newVels                      = V.zipWith uuSum (velocities planets) deltaVels
                           deltaPos                     = V.map (duProduct dt) newVels
                           newPos                       = V.zipWith uuSum (positions planets) deltaPos
                       in advance (n-1) dt (Planets newPos newVels pmasses)


sample = let p0   = U.fromList [ -1.0, 0.0, 0.0 ]
             p1   = U.fromList [  0.0, 0.0, 0.0 ]
             p2   = U.fromList [  1.0, 0.0, 0.0 ]
             positions  = V.fromList [ p0, p1, p2 ]
             v0   = U.fromList [ -1.0, 0.0, 0.0 ]
             v1   = U.fromList [  0.0, 0.0, 0.0 ]
             v2   = U.fromList [  1.0, 0.0, 0.0 ]
             velocities  = V.fromList [v0, v1, v2]
             masses = V.fromList [ 1.0, 100.0, 1.0 ]
         in (Planets positions velocities masses)


