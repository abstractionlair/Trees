module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Planets = Planets { positions :: !(V.Vector (U.Vector Double)), velocities :: !(V.Vector (U.Vector Double)), masses :: !(V.Vector Double)} deriving Show


--------------------------------------------------------------------------------
-- Vectors, in the physics sense.  Using Data.Vector.Unboxed

uuDifference :: U.Vector Double -> U.Vector Double -> U.Vector Double
uuDifference u1 u2 = U.zipWith (-) u1 u2

uuSum :: U.Vector Double -> U.Vector Double -> U.Vector Double
uuSum u1 u2 = U.zipWith (+) u1 u2

duProduct :: Double -> U.Vector Double -> U.Vector Double
duProduct d u = U.map (*d) u

udProduct :: U.Vector Double -> Double -> U.Vector Double
udProduct u d = U.map (*d) u

uMagnitudeSq :: U.Vector Double ->  Double
uMagnitudeSq u = U.sum (U.zipWith (*) u u)

uMagnitude :: U.Vector Double ->  Double
uMagnitude u = sqrt (U.sum (U.zipWith (*) u u))

uZero :: Int -> U.Vector Double
uZero n = U.replicate n (0.0::Double)

uZero3 :: U.Vector Double
uZero3 = U.replicate 3 (0.0::Double)

--------------------------------------------------------------------------------
-- Data.Vector

-- [1,2,3,4] -> [ [1,2,3,4], [2,3,4], [3,4], [4] ]
subVecsInOrder v = V.iterateN (V.length v) V.unsafeTail v

-- [1,2,3,4] -> [ f 1 2, f 1 3, f 1 4, f 1 5 ]
mapHeadToTail f v = V.map (f (V.unsafeHead v)) (V.unsafeTail v)         

allButLast v = V.unsafeSlice 0 (V.length v - 1) v

dvProduct :: Double -> V.Vector Double -> V.Vector Double
dvProduct d v = V.map (*d) v



--------------------------------------------------------------------------------
-- Nested Data.Vector

vvMap f vv = V.map (V.map f) vv

vvZipWith f v w = V.zipWith ($) (V.map (V.zipWith f) v) w

element i j = (i,j)

-- [ [ f 0 0, f 0 1, f 0 2 ],
--   [ f 1 0, f 1 1        ],
--   [ f 2 0               ] ]
makeULTD f n = let makerow row = V.generate (n-row) (f row)
               in V.generate n makerow

-- [ [ f 0 0, f 0 1, f 0 2 ],
--   [        f 1 1, f 1 2 ],
--   [               f 2 2 ] ]
makeURTD f n = let g i j = f i (j+i)
                   makerow row = V.generate (n-row) (g row) 
               in V.generate n makerow

-- [ [ f 0 1, f 0 2, f 0 3 ],
--   [        f 1 2, f 1 3 ],
--   [               f 2 3 ] ]
makeURT f n = let g i j = f i (j+i+1)
                  makerow i = V.generate (n-i) (g i)
              in V.generate n makerow 

-- [ [ f 0 0,              ]
--   [ f 1 0, f 1 1,       ]
--   [ f 2 0, f 2 1, f 2 2 ] ]
makeLLTD f n = let makerow row = V.generate (1+row) (f row)
               in V.generate n makerow

-- [ [ f 1 0,              ]
--   [ f 2 0, f 2 1,       ]
--   [ f 3 0, f 3 1, f 3 2 ] ]
makeLLT f n = let g i j = f (i+1) j 
                  makerow row = V.generate (1+row) (g row)
              in V.generate n makerow


--------------------------------------------------------------------------------
-- The upper right or lower left parts of a matrix as a Data.Vector (Data.Vector)

-- [v1,v2,v3,v4] [w1,w2,w3,w4] -> [ [ f v1 w1, f v1 w2, f v1 w3, f v1 w4 ],
--                                  [          f v2 w2, f v2 w2, f v2 w4 ],
--                                  [                   f v3 w3, f v3 w4 ],
--                                  [                            f v4 w4 ] ]
mapOuterURTAndD f v w = let funcs  = V.map f v                                           -- e.g. [ f w1,         f w2,         f w3         ]                            
                            vfuncs = V.map V.map funcs                                   --      [ V.map (f w1), V.map (f w2), V.map (f w3) ]
                            svs    = subVecsInOrder w                                    --      [ [ v1,v2,v3],  [v2,v3],      [v3]         ]
                        in V.zipWith ($) vfuncs svs

-- Avoiding diagonals
-- [v1,v2,v3,v4] [w1,w2,w3,w4] -> [ [ f v1 w2, f v1 w3, f v1 w4 ],          
--                                [            f v2 w3, f v2 w4 ],          
--                                [                     f v3 w4 ] ]
mapOuterURT f v w = mapOuterURTAndD f v ( V.tail w)

-- Same result when w = v
-- [v1,v2,v3,v4] -> [ [ f v1 v2, f v1 v3, f v1 v4 ],          
--                    [          f v2 v3, f v2 v4 ],          
--                    [                   f v3 v4 ] ]          
mapSelfOuterURT f v = V.map (mapHeadToTail f) (allButLast (subVecsInOrder v))
                                            


-- [ [ g12 g13 g14 ],   [ m2, m3, m4 ] -> [ f g12 m2 + f g13 m3 + f g14 m4,
--   [     g23 g24 ],                                  f g23 m3 + f g24 m4,
--   [         g34 ] ],                                           f g34 m4 ]
vvURTDotMapV f vv v = let vsvs = V.unsafeInit ( subVecsInOrder v )
                      in V.map (V.foldl uuSum uZero3) $ V.zipWith (V.zipWith f) vv vsvs


-- vvDotMapV f vv v = let rowFunc row = V.foldl uuSum uZero3 $ V.zipWith f row v
--                    in V.map rowFunc vv
-- vvDotMapV f vv v = let rowFunc row = V.foldl (+) 0.0 $ V.zipWith f row v
--                    in V.map rowFunc vv


-- [ [ g00 g01 g02],  [m0, m1, m2] -> [ [ f g00 m0, f g01 m1, f g02 m2 ],
--   [ g10 g11 g12],                    [ f g10 m0, f g11 m1, f g12 m2 ],
--   [ g20 g21 g22] ],                  [ f g20 m0, f g21 m1, f g22 m2 ] ]
vvZipWithV f vv v = V.map (V.zipWith (flip f) v) vv

vvDotMapV f vv v = V.map (V.foldl uuSum uZero3) $ vvZipWithV f vv v


-- [ [ g12 g13 g14 ],   [ m1, m2, m3 ] -> [ f m1 g12,
--   [     g23 g24 ],                       f m1 g13 + f m2 g23,
--   [         g34 ] ],                     f m1 g14 + f m2 g24 + f m3 g34 ]
vvURTTransDotMapV f vv v = let funcs = V.map f v                                        -- [ f v2, f v3, f v4 ]
                               vvvs  = V.zipWith V.map funcs vv                         -- [ [ f v2 vv1, f v2 vv2, f v2 vv3 ], [ f v3 vv2, f v3 vv3 ], [ f v4 vv3 ] ]
                           in vvvs
                                     
-- Not really URT specific 
-- Monoid?
vvAccumAcrossURT f zero v = V.map (V.foldl f zero) v

vvAccumDownURT f v = case (V.length v) of
                          0 -> V.empty
                          1 -> V.head v
                          _ -> let r  = V.head v
                                   rs = V.tail v
                               in (V.slice 0 1 r) V.++ (V.zipWith f (V.tail r) (vvAccumDownURT f rs))



--------------------------------------------------------------------------------
-- Do something

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


advance n dt planets = let --deltaXs                 = mapSelfOuterURT uuDifference (positions planets)
                           --invDistancesCubed       = vvMap ((**(-3)).sqrt.uMagnitudeSq) deltaXs
                           --deltaXOverDistanceCubed = vvZipWith duProduct invDistancesCubed deltaXs
                           massesDt                  = dvProduct dt (masses planets)
                           --gmURT                   = vvURTDotMapV udProduct deltaXOverDistanceCubed massesDt
                           --gmLLT                   = vvAccumDownURT uuSum (vvURTTransDotMapV duProduct deltaXOverDistanceCubed massesDt)
                           gURT                      = gravURT planets
                           gLLT                      = gravLLT gURT
                           gmURT                     = vvDotMapV udProduct gURT (V.unsafeTail massesDt)
                           gmLLT                     = vvDotMapV udProduct gLLT massesDt
                           nPlanets                  = V.length massesDt
                           lastPlanet                = nPlanets - 1
                           accel i | i == 0          = gmURT V.! 0
                                   | i == lastPlanet = gmLLT V.! (lastPlanet-1)
                                   | otherwise       = (gmURT V.! i) `uuSum` (gmLLT V.! (i-1))
                           accels = V.generate nPlanets accel
                           --accels                  = V.zipWith uuDifference (V.snoc gmURT uZero3) (V.cons uZero3 gmLLT)
                           newVels                 = V.zipWith uuSum (velocities planets) accels
                       in (gURT, gLLT, gmURT, gmLLT, nPlanets, lastPlanet, accels, newVels)






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
