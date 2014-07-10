module NewOrd
       where

data MyPair a b = MyPair { key :: a, value :: b }

instance (Eq a) => Eq ( MyPair a b ) where
         (==) p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) = k1 == k2
         (/=) p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) = k1 /= k2

instance (Ord a) => Ord ( MyPair a b ) where
         (<)  p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) = k1 < k2
         (<=) p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) = k1 <= k2
         (>=) p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) = k1 >= k2
         (>)  p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) = k1 > k2
         max  p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) | k1 > k2   = p1
                                                      | otherwise = p2
         min  p1@( MyPair k1 v1 ) p2@( MyPair k2 v2 ) | k1 < k2   = p1
                                                      | otherwise = p2

