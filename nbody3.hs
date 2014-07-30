module Main where

import Control.DeepSeq
import qualified Data.Vector.Unboxed as U


sample = let dt         = 0.01 :: Double
             nBodies    = 2::Int
             nAxes      = 3::Int
             masses     = ( U.fromList [  1.0, 2.0 ] ) :: U.Vector(Double)
             positions  = ( U.fromList [  1.0, 1.0, 1.0,  0.0, 0.0, 0.0 ] ) :: U.Vector(Double)
             velocities = ( U.fromList [  0.0, 0.0, 0.0,  1.0, 1.0, 1.0 ] ) :: U.Vector(Double)
         in (dt,nBodies,nAxes,masses,positions,velocities)

advance count dt nBodies nAxes masses positions velocities =
  let vecSize                 = nBodies * nAxes
      nBodiesM1               = nBodies - 1
      mass      b             = masses U.! b
      offset    body axis     = body * nAxes + axis
      invOffset oset          = oset `divMod` nAxes

      advanceVels (b,b2) (posns, vels ) = let pos b a     = posns U.! (offset b a)
                                              vel b a     = vels  U.! (offset b a)
                                              deltaX      = U.generate 3 (\i -> (pos b i) - (pos b2 i))
                                              distance    = sqrt ( (deltaX U.! 0) * (deltaX U.! 0) + (deltaX U.! 1) * (deltaX U.! 1) + (deltaX U.! 2) * (deltaX U.! 2) )
                                              mag         = dt / ( distance * distance * distance )
                                              velFor oset = let (b3,a) = invOffset oset
                                                            in if b3 == b
                                                               then (vel b  a) - (deltaX U.! a) * (mass b2) * mag
                                                               else if b3 == b2
                                                               then (vel b2 a) + (deltaX U.! a) * (mass b)  * mag
                                                               else (vel b3 a)
                                          in (posns, U.generate vecSize velFor)
      advanceAllVels                    = foldl (.) id $ map advanceVels [ (i,j) | i <- [0..nBodiesM1], j <- [(i+1)..nBodiesM1] ]

      advancePos posns  vels            = let pos b a     = posns U.! (offset b a)
                                              vel b a     = vels  U.! (offset b a)
                                              posFor oset = let (b,a) = invOffset oset
                                                            in (pos b a) + dt * (vel b a)
                                          in U.generate vecSize posFor

      advance' 0     positions velocities = (positions, velocities)
      advance' count positions velocities =
        let 
            (_,newVels) = advanceAllVels (positions,velocities)
            newPos      = advancePos positions newVels
        in newPos `deepseq` (advance' (count-1) newPos newVels )

  in advance' count positions velocities 

