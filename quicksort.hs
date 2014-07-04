bucket pivot [left, middle, right] value | value <  pivot = [ value : left,        middle,         right ]
                                         | value == pivot = [         left, value: middle,         right ]
                                         | value >  pivot = [         left,        middle, value : right ]

partition pivot leftMiddleRight []     =  leftMiddleRight
partition pivot leftMiddleRight (x:xs) =  partition pivot ( bucket_pivot leftMiddleRight x ) xs
                                          where bucket_pivot = bucket pivot

partition_first []     = [ [], [], [] ]
partition_first (x:xs) = partition x [ [], [], [] ] (x:xs)

reverse_prepend []     []     = []
reverse_prepend (x:xs) []     = (x:xs)
reverse_prepend []     (y:ys) = reverse_prepend [y] ys
reverse_prepend (x:xs) (y:ys) = reverse_prepend ( y:x:xs) ys
backwards                     = reverse_prepend []
join x y                      = reverse_prepend y (backwards x )

my_fold :: ( b -> a -> b ) -> b -> [a]    -> b
my_fold    f                  x    []     =  x
my_fold    f                  x    (y:ys) =  ( my_fold f ) ( f x y ) ys

join_many = my_fold join []

sort []     = []
sort (x:[]) = x:[]
sort (x:xs) = join_many (  sort_ends ( partition_first (x:xs) ) )
              where sort_ends [ left, middle, right ] = [ sort( left ), middle, sort( right ) ]












{- 
  my_fold f [0] [ 1, 2, 3 ]
  my_fold f (f 0 1) [ 2, 3 ]
  my_fold f (f (f 0 1) 2 ) [ 3 ]
  my_fold f (f (f (f 0 1) 2 ) 3) []
  f (f (f (f 0 1) 2 ) 3)
  if f is (-)
  (((0 - 1) - 2 ) -3)
  ((-1 - 2 ) -3)
  (-3 -3)
  -6
-}


-- join_many0 :: [a] -> [ [a] ] -> [a]
-- join_many0 []     []     = []
-- join_many0 (x:xs) []     = (x:xs)
-- join_many0 []     (y:ys) = join_many0 y ys
-- join_many0 (x:xs) (y:ys) = join_many0 ( join (x:xs) y ) ys
-- join_many x              = join_many0 [] x



-- partition pivot [ left, middle, right ] []     =  [ left, middle, right ]
-- partition pivot [ left, middle, right ] (x:xs) =  partition pivot ( bucket pivot [ left, middle, right ] x ) xs



--backwards0 []     []     = ( [], [] )
--backwards0 (x:xs) []     = ( x:xs, [] )
--backwards0 []     (y:ys) = backwards0 [y] ys 
--backwards0 (x:xs) (y:ys) = backwards0 (y:x:xs)  ys
--backwards x              = fst ( backwards0 [] x )
-- reverse_prepend []     (y:ys) = backwards (y:ys)   -- The general case of reverse_prepend will reverse the second list so we need to do that here
