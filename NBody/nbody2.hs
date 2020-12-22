module Main where

import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Environment
import Text.Printf

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

uMagnitudeSq :: U.Vector Double ->  Double
uMagnitudeSq u = U.sum (U.zipWith (*) u u)

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

vvMap f vv = V.map (V.map f) vv

-- [ [ g00 g01 g02],  [m0, m1, m2] -> [ [ f g00 m0, f g01 m1, f g02 m2 ],
--   [ g10 g11 g12],                    [ f g10 m0, f g11 m1, f g12 m2 ],
--   [ g20 g21 g22] ],                  [ f g20 m0, f g21 m1, f g22 m2 ] ]
vvZipWithV f vv v = V.map (V.zipWith (flip f) v) vv

-- [ [ g00 g01 g02],  [m0, m1, m2] -> [ [ f g00 m0 `uuSum` f g01 m1 `uuSum` f g02 m2 ],
--   [ g10 g11 g12],                    [ f g10 m0 `uuSum` f g11 m1 `uuSum` f g12 m2 ],
--   [ g20 g21 g22] ],                  [ f g20 m0 `uuSum` f g21 m1 `uuSum` f g22 m2 ] ]
vvDotMapV f vv v = V.map (V.foldl' uuSum uZero3) $ vvZipWithV f vv v

-- [ [ g00 g01 g02 ],   [ m0, m1, m2 ] -> [ f g00 m0 + f g01 m1 + f g02 m2,
--   [     g11 g12 ],                                  f g11 m1 + f g12 m2,
--   [         g22 ] ],                                           f g22 m2 ]
vvURTDotMapV f vv v = let vsvs = subVecsInOrder v
                         in V.map (V.foldl' uuSum uZero3) $ V.zipWith (V.zipWith f) vv vsvs

--------------------------------------------------------------------------------
-- Physics

-- The positional part of the force on i due to j.
-- \delta\vec{x} / |\delta\vec{x}|^3 
grav positions i j = let dx   = (positions V.! j) `uuDifference` (positions V.! i)
                         idxc = 1.0 / (uMagnitude dx) ^ 3
                     in dx `udProduct` idxc

-- The upper right triangle of the forces between planets.
-- Matrix of vectors.
gravURT positions = makeURT (grav positions) ((V.length positions)-1)

-- The lower left triangle
-- From the upper right using the fact that the matrix is antisymmetric
gravLLT urt = let f i j = (urt V.! j V.! (i-j)) `udProduct` (-1.0)
               in makeLLTD f (V.length urt)

kenergy :: V.Vector(Double) -> V.Vector (U.Vector Double) -> Double
kenergy masses velocities = let vvs  = V.map uMagnitudeSq velocities
                                mv2s = V.zipWith (*) vvs masses
                            in 0.5 * V.sum mv2s

penergy' :: V.Vector(Double) -> V.Vector (U.Vector Double) -> Int -> Int -> Double
penergy' masses positions i j = let pi = positions V.! i
                                    pj = positions V.! j
                                    d  = uMagnitude $ pi `uuDifference` pj
                                    mi = masses V.! i
                                    mj = masses V.! j
                                in -1.0 * mi * mj / d

penergy :: V.Vector(Double) -> V.Vector (U.Vector Double) -> Double
penergy masses positions = let nm1 = (V.length masses) - 1
                           in sum [ penergy' masses positions i j | i <- [0..nm1], j <- [i+1..nm1] ]

energy :: V.Vector(Double) -> V.Vector (U.Vector Double) -> V.Vector (U.Vector Double) -> Double
energy masses positions velocities = (kenergy masses velocities) + (penergy masses positions)

momentum :: V.Vector(Double) -> V.Vector (U.Vector Double) -> U.Vector Double
momentum masses velocities = let mvs = V.zipWith udProduct velocities masses
                             in V.foldl' uuSum uZero3 mvs

advance :: Int -> Double -> V.Vector Double -> V.Vector (U.Vector Double) -> V.Vector (U.Vector Double) -> (V.Vector (U.Vector Double), V.Vector (U.Vector Double))
advance n dt masses positions velocities = let nPlanets                   = V.length rmasses
                                               lastPlanet                 = nPlanets - 1
                                               rmasses                    = dvProduct (dt * dt) masses
                                               rmassesTail                = V.unsafeTail rmasses
                                               rvelocities                = V.map (duProduct dt) velocities
                                               
                                               advance' :: Int -> V.Vector (U.Vector Double) -> V.Vector (U.Vector Double) -> (V.Vector (U.Vector Double), V.Vector (U.Vector Double))
                                               advance'  0 posns vels = (posns, vels)
                                               advance' n posns vels = let gURT                         = gravURT posns
                                                                           gLLT                         = gravLLT gURT
                                                                           gmURT                        = vvURTDotMapV udProduct gURT rmassesTail
                                                                           gmLLT                        = vvDotMapV udProduct gLLT rmasses
                                                                           deltaVel i | i == 0          = gmURT V.! 0
                                                                                      | i == lastPlanet = gmLLT V.! (lastPlanet-1)
                                                                                      | otherwise       = (gmURT V.! i) `uuSum` (gmLLT V.! (i-1))
                                                                           deltaVels                    = V.generate nPlanets deltaVel
                                                                           newVels                      = V.zipWith uuSum vels deltaVels 
                                                                           newPosns                     = V.zipWith uuSum posns newVels
                                                                       in newPosns `deepseq` (advance' (n-1) newPosns newVels)

                                               (fpositions, frvelocities) = advance' n positions rvelocities
                                               fvelocities                = V.map (duProduct (1.0/dt)) frvelocities
                                           in (fpositions, fvelocities)



--------------------------------------------------------------------------------


                               

main = do
     let days_per_year = 365.24::Double
     let solar_mass    = (4 * pi ^ 2)::Double
     let dp            = days_per_year::Double
     let dt            = 0.01::Double

     let sunPos      = U.fromList [ 0, 0, 0 ]
     let sunVel      = U.fromList [ 0, 0, 0 ]
     let sunMass     = solar_mass

     let jupiterPos  = U.fromList [ 4.84143144246472090e+00,      -1.16032004402742839e+00,      -1.03622044471123109e-01      ] :: U.Vector(Double)
     let jupiterVel  = U.fromList [ 1.66007664274403694e-03 * dp,  7.69901118419740425e-03 * dp, -6.90460016972063023e-05 * dp ] :: U.Vector(Double)
     let jupiterMass = 9.54791938424326609e-04 * solar_mass

     let saturnPos   = U.fromList [  8.34336671824457987e+00,      4.12479856412430479e+00,      -4.03523417114321381e-01      ] :: U.Vector(Double)
     let saturnVel   = U.fromList [ -2.76742510726862411e-03 * dp, 4.99852801234917238e-03 * dp,  2.30417297573763929e-05 * dp ] :: U.Vector(Double)
     let saturnMass  = 2.85885980666130812e-04 * solar_mass

     let uranusPos   = U.fromList [ 1.28943695621391310e+01,      -1.51111514016986312e+01,      -2.23307578892655734e-01      ] :: U.Vector(Double)
     let uranusVel   = U.fromList [ 2.96460137564761618e-03 * dp,  2.37847173959480950e-03 * dp, -2.96589568540237556e-05 * dp ] :: U.Vector(Double)
     let uranusMass  = 4.36624404335156298e-05 * solar_mass

     let neptunePos  = U.fromList [ 1.53796971148509165e+01,      -2.59193146099879641e+01,       1.79258772950371181e-01      ] :: U.Vector(Double)
     let neptuneVel  = U.fromList [ 2.68067772490389322e-03 * dp,  1.62824170038242295e-03 * dp, -9.51592254519715870e-05 * dp ] :: U.Vector(Double)
     let neptuneMass = 5.15138902046611451e-05 * solar_mass

     let positions  = V.fromList [ sunPos,  jupiterPos,  saturnPos,  uranusPos,  neptunePos ]
     let velocities = V.fromList [ sunVel,  jupiterVel,  saturnVel,  uranusVel,  neptuneVel ]
     let masses     = V.fromList [ sunMass, jupiterMass, saturnMass, uranusMass, neptuneMass ]

     n <- getArgs >>= readIO.head :: IO Int
     let mo                = momentum masses velocities
     print (energy masses positions velocities )
     let offsetSunVel      = udProduct mo (-1.0 / solar_mass )
     let offsetVelocities  = V.fromList [ offsetSunVel, jupiterVel,  saturnVel,  uranusVel, neptuneVel ]
     print (energy masses positions offsetVelocities)
     let (positions', velocities') = advance n dt masses positions offsetVelocities
     print (energy masses positions velocities' )










-- sample = let p0          = U.fromList [ -1.0,   0.0, 0.0::Double ]
--              p1          = U.fromList [  0.0,   0.0, 0.0::Double ]
--              p2          = U.fromList [  1.0,   0.0, 0.0::Double ]
--              positions   = V.fromList [   p0,    p1,  p2 ]
--              v0          = U.fromList [ -1.0,   0.0, 0.0::Double ]
--              v1          = U.fromList [  0.0,   0.0, 0.0::Double ]
--              v2          = U.fromList [  1.0,   0.0, 0.0::Double ]
--              velocities  = V.fromList [   v0,    v1,  v2 ]
--              masses      = V.fromList [  1.0, 100.0, 1.0::Double ]
--          in (masses, positions, velocities)









-- advance 0 _  masses positions velocities = (positions, velocities)
-- advance n dt masses positions velocities = let massesDt                     = dvProduct dt masses
--                                                gURT                         = gravURT positions
--                                                gLLT                         = gravLLT gURT
--                                                gmURT                        = vvURTDotMapV udProduct gURT (V.unsafeTail massesDt)
--                                                gmLLT                        = vvDotMapV udProduct gLLT massesDt
--                                                nPlanets                     = V.length massesDt
--                                                lastPlanet                   = nPlanets - 1
--                                                deltaVel i | i == 0          = gmURT V.! 0
--                                                           | i == lastPlanet = gmLLT V.! (lastPlanet-1)
--                                                           | otherwise       = (gmURT V.! i) `uuSum` (gmLLT V.! (i-1))
--                                                deltaVels                    = V.generate nPlanets deltaVel
--                                                newVels                      = V.zipWith uuSum velocities deltaVels
--                                                deltaPos                     = V.map (duProduct dt) newVels
--                                                newPos                       = V.zipWith uuSum positions deltaPos
--                                            in advance (n-1) dt masses newPos newVels
