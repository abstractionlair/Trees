module Main where

import System.Random

main = do
  g <- getStdGen
  print $ take 20 (randoms g :: [Double])



make_generator seed = mkStdGen seed

one_random_int gen = let (randomNumber,  newGen) = (random gen) :: (Int, StdGen)
                     in randomNumber

two_random_ints gen = let (randomNumber1,  gen2) = (random gen) :: (Int, StdGen)
                          (randomNumber2,  gen3) = (random gen2) :: (Int, StdGen)
                      in (randomNumber1, randomNumber2)

n_random_ints gen n = take n $ (randoms gen) :: [Int]

infinite_random_ints gen = (randoms gen) :: [Int]

one_random_int_in_range gen = let (randomNumber,  newGen) = (randomR (10,20) gen) :: (Int, StdGen)
                              in randomNumber

two_random_ints_in_range gen = let (randomNumber1,  gen2) = (randomR (10,20) gen) :: (Int, StdGen)
                                   (randomNumber2,  gen3) = (randomR (10,20) gen2) :: (Int, StdGen)
                              in (randomNumber1,randomNumber2)

n_random_ints_in_range gen n = take n $ (randomRs ((-50),50) gen) :: [Int]

infinite_random_ints_in_range gen = (randomRs (100,200) gen) :: [Int]


-- Random seed, but only once
get_random_generator = do
                         getStdGen

get_new_random_generator = do
                             g <- newStdGen
                             return g


n_really_random_ints_in_range n = do
                                    g <- newStdGen
                                    let rs = (randomRs (1,100) g) :: [Int]
                                    let nrs = take n rs
                                    return nrs

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

-- newStdGen gives us an IO StdGen not a StdGen
-- we can't use it outside of a do block (or bind,lift etc. manually)
-- within a do block it will be passed as just StdGen to functions that want that
shuffle2 :: [a] -> IO [a]
shuffle2 xs = do
                g <- newStdGen
                return (shuffle g xs)

-- Similar for random numbers from an IO StdGen
-- You get for instance IO [Int] rather than [Int] and need to use them in a do block too (or ... )
shuffle_random n = do
                     ns <- n_really_random_ints_in_range n
                     shuffle2 ns
