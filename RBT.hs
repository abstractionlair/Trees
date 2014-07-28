module RBT where

import BinTree
import BST


data RBColor = Red | Black deriving Show

data RBItem a = RBItem { color :: RBColor, value :: a } deriving Show

instance (Eq a) => Eq (RBItem a) where
         (==)    (RBItem c1 v1)    (RBItem c2 v2)             = v1 == v2
         (/=)    (RBItem c1 v1)    (RBItem c2 v2)             = v1 /= v2
instance (Ord a) => Ord (RBItem a) where
         (<)     (RBItem c1 v1)    (RBItem c2 v2)             = v1 <  v2
         (<=)    (RBItem c1 v1)    (RBItem c2 v2)             = v1 <= v2
         (>=)    (RBItem c1 v1)    (RBItem c2 v2)             = v1 >= v2
         (>)     (RBItem c1 v1)    (RBItem c2 v2)             = v1 >  v2
         max  p1@(RBItem c1 v1) p2@(RBItem c2 v2) | v1 >  v2  = p1
                                                  | otherwise = p2
         min  p1@(RBItem c1 v1) p2@(RBItem c2 v2) | v1 < v2   = p1
                                                  | otherwise = p2

type RBT a = BST a


make_black (BinTree (RBItem cvt vvt)  rt lt) = BinTree (RBItem Black vvt) rt lt


add_to_rbt :: Ord a => RBT (RBItem a) -> a -> RBT (RBItem a)
add_to_rbt t nv = make_black . add_to_rbt' t $ RBItem Red nv


add_to_rbt' :: Ord a => RBT (RBItem a) -> (RBItem a) -> RBT (RBItem a)

add_to_rbt' EmptyBinTree         nv             = BinTree  nv EmptyBinTree        EmptyBinTree
add_to_rbt' ( BinTree vt lt rt ) nv | nv > vt   = balanceR vt lt                  (add_to_rbt' rt nv)
                                    | otherwise = balanceL vt (add_to_rbt' lt nv) rt


balanceL :: Ord a => RBItem a -> RBT (RBItem a) -> RBT (RBItem a) -> RBT (RBItem a)
balanceR :: Ord a => RBItem a -> RBT (RBItem a) -> RBT (RBItem a) -> RBT (RBItem a)

balanceL vt    (BinTree vlt@(RBItem Red vvlt)     llt@(BinTree (RBItem Red vvllt) lllt rllt) rlt) rt = BinTree vlt (make_black llt )                  (BinTree vt rlt rt)                 -- Red Left  and Red Left  Left
balanceL vt    (BinTree vlt@(RBItem Red vvlt) llt rlt@(BinTree (RBItem Red vvrlt) lrlt rrlt)    ) rt = BinTree vt  (BinTree vlt llt (make_black rlt)) rt                                  -- Red Left  and Red Right Left
balanceL vt lt                                                                                    rt = BinTree vt  lt                                 rt
balanceR vt lt (BinTree vrt@(RBItem Red vvrt) lrt rrt@(BinTree (RBItem Red vvrrt) lrrt rrrt)    )    = BinTree vrt (BinTree vt lt lrt)                (make_black rrt)                    -- Red Right and Red Right Right
balanceR vt lt (BinTree vrt@(RBItem Red vvrt)     lrt@(BinTree (RBItem Red vvlrt) llrt rlrt) rrt)    = BinTree vt  lt                                 (BinTree vrt (make_black lrt) rrt)  -- Red Right and Red Left  Right
balanceR vt lt                                                                                    rt = BinTree vt  lt                                 rt

make_rbt :: Ord a => [ a ] -> RBT ( RBItem a )
make_rbt = foldl add_to_rbt EmptyBinTree



-- Use of BST and BinTree functions

rbt_as_bst :: RBT a -> BST a
rbt_as_bst x = x

rbt_as_bin_tree :: RBT a -> BinTree a
rbt_as_bin_tree x = x

rbt_rows = bin_tree_rows . rbt_as_bin_tree

rbt_show :: Show a => RBT (RBItem a) -> [[[Char]]]
rbt_show = map ( map rbt_show' ) . rbt_rows

rbt_value = RBT.value . BinTree.value -- Get the value from inside the RBItem inside the RBT

rbt_show' EmptyBinTree = "E"
rbt_show' h            = show $ rbt_value h

search_rbt :: Ord a => a -> RBT (RBItem a) -> RBT (RBItem a)
search_rbt v t = bst_search (RBItem Black v) t




-- Testing
-- case1 = let llt = BinTree (RBItem Red 5 ) EmptyBinTree EmptyBinTree
--             lt  = BinTree (RBItem Red 10) llt          EmptyBinTree
--             rt  = EmptyBinTree
--             vt  = RBItem Black 20
--         in balL vt lt rt

-- case2 = let rlt = BinTree (RBItem Red 10) EmptyBinTree EmptyBinTree
--             lt  = BinTree (RBItem Red 0)  EmptyBinTree rlt
--             rt  = EmptyBinTree
--             vt  = RBItem Black 20
--         in balL vt lt rt

-- case3 = let rrt = BinTree (RBItem Red 20) EmptyBinTree EmptyBinTree
--             rt  = BinTree (RBItem Red 10) EmptyBinTree rrt
--             lt  = EmptyBinTree
--             vt  = RBItem Black 0
--         in balR vt lt rt

-- case4 = let lrt = BinTree (RBItem Red 10) EmptyBinTree EmptyBinTree
--             rt  = BinTree (RBItem Red 20) lrt          EmptyBinTree
--             lt  = EmptyBinTree
--             vt  = RBItem Black 0
--         in balR vt lt rt
