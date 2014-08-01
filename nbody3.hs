module Main where

import Control.DeepSeq
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import System.Environment


advance count dt nBodies nAxes masses positions velocities =
  let vecSize                 = nBodies * nAxes
      nBodiesM1               = nBodies - 1
      offset    body axis     = body * nAxes + axis
      invOffset oset          = oset `divMod` nAxes

      -- Build a function for the change in velocities from the interaction of b abd b2. (Positions go along for the ride to make the results composeable.)
      -- Get as much done outside the loop as possible.
      advanceVelsFn (b,b2) = let ob_0  = offset b 0
                                 ob_1  = ob_0 + 1
                                 ob_2  = ob_1 + 1
                                 ob2_0 = offset b2 0
                                 ob2_1 = ob2_0 + 1
                                 ob2_2 = ob2_1 + 1
  
                                 -- Build a function for the change in one component of velocities
                                 velForFn oset = let (b3,a) = invOffset oset
                                                 in if b3 == b
                                                    then let vf (vels, dx, dy, dz, masses, mag) = case a of
                                                                                                       0 -> (vels U.! (ob_0    )) - dx * (masses U.! b2) * mag
                                                                                                       1 -> (vels U.! (ob_0 + 1)) - dy * (masses U.! b2) * mag
                                                                                                       2 -> (vels U.! (ob_0 + 2)) - dz * (masses U.! b2) * mag
                                                         in vf
                                                    else if b3 == b2
                                                    then let vf (vels, dx, dy, dz, masses, mag) = case a of 
                                                                                                       0 -> (vels U.! (ob2_0    )) + dx * (masses U.! b)  * mag
                                                                                                       1 -> (vels U.! (ob2_0 + 1)) + dy * (masses U.! b)  * mag
                                                                                                       2 -> (vels U.! (ob2_0 + 2)) + dz * (masses U.! b)  * mag
                                                         in vf
                                                    else let vf (vels, dx, dy, dz, masses, mag) = (vels U.! oset)
                                                         in vf 

                                 -- Collect functions for all the components
                                 velForFns     = V.generate vecSize velForFn

                                 -- Put them together into the single function for all the components
                                 advanceVels' (posns, vels) = let dx       = (posns U.! ob_0) - (posns U.! ob2_0)
                                                                  dy       = (posns U.! ob_1) - (posns U.! ob2_1)
                                                                  dz       = (posns U.! ob_2) - (posns U.! ob2_2)
                                                                  distance = sqrt ( dx * dx + dy * dy + dz * dz )
                                                                  mag      = dt / ( distance * distance * distance )
                                                              in (posns, (U.convert $ V.map ($ (vels, dx, dy, dz, masses, mag)) velForFns ) :: U.Vector(Double))

                             in advanceVels'
      -- Collect functions for all (b,b2) pairs and join them into a single velocity advancing function
      advanceAllVels       = foldl (.) id $ map advanceVelsFn [ (i,j) | i <- [0..nBodiesM1], j <- [(i+1)..nBodiesM1] ]

      advance' 0     positions velocities = (positions, velocities)
      advance' count positions velocities =
        let 
            (_,newVels) = advanceAllVels (positions,velocities)
            newPos      = U.zipWith (\p v -> p + v * dt) positions newVels
        in newPos `deepseq` (advance' (count-1) newPos newVels )

  in advance' count positions velocities 

--------------------------------------------------------------------------------
-- Called very few times; not optimized.

kenergy nBodies nAxes masses velocities = let vvs = U.map (^2) velocities
                                              mmm = foldl (U.++) (U.empty :: U.Vector(Double)) $ map (\x -> (U.fromList [x,x,x])) (U.toList masses)
                                              mv2s = U.zipWith (*) vvs mmm
                                          in 0.5 * U.sum mv2s

penergy' nBodies nAxes masses positions i j = let offset body axis = body * nAxes + axis
                                                  ix = positions U.! (offset i 0)
                                                  iy = positions U.! (offset i 1)
                                                  iz = positions U.! (offset i 2)
                                                  jx = positions U.! (offset j 0)
                                                  jy = positions U.! (offset j 1)
                                                  jz = positions U.! (offset j 2)
                                                  dx = (ix-jx)
                                                  dy = (iy-jy)
                                                  dz = (iz-jz)
                                                  d = sqrt( dx*dx + dy*dy + dz*dz )
                                                  mi = masses U.! i
                                                  mj = masses U.! j
                                              in -1.0 * mi * mj / d

penergy nBodies nAxes masses positions = let nm1 = nBodies - 1
                                         in sum [ penergy' nBodies nAxes masses positions i j | i <- [0..nm1], j <- [i+1..nm1] ]

energy nBodies nAxes masses positions velocities = (kenergy nBodies nAxes masses velocities) + (penergy nBodies nAxes masses positions)

momentum nBodies nAxes masses velocities = let mvx = U.sum $ U.zipWith (*) masses $ U.ifilter (\i v -> (i `mod` nAxes == 0)) velocities
                                               mvy = U.sum $ U.zipWith (*) masses $ U.ifilter (\i v -> (i `mod` nAxes == 1)) velocities
                                               mvz = U.sum $ U.zipWith (*) masses $ U.ifilter (\i v -> (i `mod` nAxes == 2)) velocities
                                           in U.fromList [ mvx, mvy, mvz ]
--------------------------------------------------------------------------------





main = do
     let days_per_year = 365.24::Double
     let solar_mass    = (4 * pi ^ 2)::Double
     let dp            = days_per_year::Double
     let dt            = 0.01::Double
     let nBodies       = 5
     let nAxes         = 3

     let positions = U.fromList [ 0,                              0,                             0,                                             -- Sun
                                  4.84143144246472090e+00,       -1.16032004402742839e+00,      -1.03622044471123109e-01,                       -- Jupiter
                                  8.34336671824457987e+00,        4.12479856412430479e+00,      -4.03523417114321381e-01,                       -- Saturn
                                  1.28943695621391310e+01,       -1.51111514016986312e+01,      -2.23307578892655734e-01,                       -- Uranus
                                  1.53796971148509165e+01,       -2.59193146099879641e+01,       1.79258772950371181e-01 ] :: U.Vector(Double)  -- Neptune



     let velocities0= U.fromList [  0,                            0,                             0,                                                 -- Sun
                                    1.66007664274403694e-03 * dp, 7.69901118419740425e-03 * dp, -6.90460016972063023e-05 * dp,                      -- Jupiter
                                   -2.76742510726862411e-03 * dp, 4.99852801234917238e-03 * dp,  2.30417297573763929e-05 * dp,                      -- Saturn
                                    2.96460137564761618e-03 * dp, 2.37847173959480950e-03 * dp, -2.96589568540237556e-05 * dp,                      -- Uranus
                                    2.68067772490389322e-03 * dp, 1.62824170038242295e-03 * dp, -9.51592254519715870e-05 * dp ] :: U.Vector(Double) -- Neptune



     let masses = U.fromList [                           solar_mass,                      -- Sun
                               9.54791938424326609e-04 * solar_mass,                      -- Jupiter
                               2.85885980666130812e-04 * solar_mass,                      -- Saturn
                               4.36624404335156298e-05 * solar_mass,                      -- Uranus
                               5.15138902046611451e-05 * solar_mass ] :: U.Vector(Double) -- Neptune


     let mo = momentum nBodies nAxes masses velocities0
     let sunVel = U.map (* (-1.0/solar_mass)) mo
     let velocities = sunVel U.++ (U.drop 3 velocities0)
     print (energy nBodies nAxes masses positions velocities)

     n <- getArgs >>= readIO.head :: IO Int
     let (positions', velocities') = advance n dt nBodies nAxes masses positions velocities
     print (energy nBodies nAxes masses positions' velocities')
