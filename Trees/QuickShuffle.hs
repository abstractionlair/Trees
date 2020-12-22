module QuickShuffle
       where

import System.Random

rpartition g xs = rpartition' g [] [] xs
rpartition' g left right [] = (left,right,g)
rpartition' g left right (x:xs) = let (r,ng) = (random g)::(Bool,StdGen)
                                  in if r
                                     then rpartition' ng (x:left) right     xs
                                     else rpartition' ng left     (x:right) xs
shuffle :: StdGen -> [a] -> [a]
shuffle g []     = []
shuffle g (x:[]) = [x]
shuffle g xs     = let (left,right,g2) = rpartition g xs
                       (g3,g4) = split g2
                   in (shuffle g3 left) ++ (shuffle g3 right)
