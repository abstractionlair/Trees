module RBT
       where

import BinTree
import BST


data RBColor = Red | Black deriving Show

data RBItem a = RBItem { color :: RBColor, value :: a } deriving Show

instance (Eq a) => Eq ( RBItem a ) where
         (==) p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) = v1 == v2
         (/=) p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) = v1 /= v2

instance (Ord a) => Ord ( RBItem a ) where
         (<)  p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) = v1 <  v2
         (<=) p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) = v1 <= v2
         (>=) p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) = v1 >= v2
         (>)  p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) = v1 >  v2
         max  p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) | v1 >  v2  = p1
                                                      | otherwise = p2
         min  p1@( RBItem c1 v1 ) p2@( RBItem c2 v2 ) | v1 < v2   = p1
                                                      | otherwise = p2
type RBT a = BST a

rbt_as_bst :: RBT a -> BST a
rbt_as_bst x = x

rbt_as_bin_tree :: RBT a -> BinTree a
rbt_as_bin_tree x = x

make_black t@(BinTree vt@(RBItem cvt vvt)  rt lt) = BinTree (RBItem Black vvt) rt lt

add_to_rbt :: Ord a => RBT ( RBItem a ) -> a -> RBT ( RBItem a )
add_to_rbt t nv = make_black ( add_to_rbt' t ( RBItem Red nv ) )

add_to_rbt' :: Ord a => RBT (RBItem a) -> (RBItem a) -> RBT (RBItem a)
add_to_rbt' EmptyBinTree           nv             = BinTree nv EmptyBinTree EmptyBinTree
add_to_rbt' t@( BinTree vt lt rt ) nv | nv > vt   = balanceR vt lt                  (add_to_rbt' rt nv)
                                      | otherwise = balanceL vt (add_to_rbt' lt nv) rt


-- Left Breakdown in as patterns
--       vt    lt                                                                                                                                       rt
--       vt    lt@(BinTree vlt                    llt                                               rlt                                               ) rt
--       vt    lt@(BinTree vlt@(RBItem cvlt vvlt) llt@(BinTree vllt                      lllt rllt) rlt@(BinTree vrlt                      lrlt rrlt )) rt
--       vt    lt@(BinTree vlt@(RBItem cvlt vvlt) llt@(BinTree vllt@(RBItem cvllt vvllt) lllt rllt) rlt@(BinTree vrlt@(RBItem cvrlt vvrlt) lrlt rrlt )) rt
-- Right Breakdown in as patterns
--       vt lt rt@(BinTree vrt                    lrt                                               rrt                                               )
--       vt lt rt@(BinTree vrt@(RBItem cvrt vvrt) lrt@(BinTree vlrt                      llrt rlrt) rrt@(BinTree vrrt                      llrt rlrt ))
--       vt lt rt@(BinTree vrt@(RBItem cvrt vvrt) lrt@(BinTree vlrt@(RBItem cvlrt vvlrt) llrt rlrt) rrt@(BinTree vrrt@(RBItem cvrrt vvrrt) llrt rlrt ))


balanceL :: Ord a => RBItem a -> RBT (RBItem a) -> RBT (RBItem a) -> RBT (RBItem a)
balanceR :: Ord a => RBItem a -> RBT (RBItem a) -> RBT (RBItem a) -> RBT (RBItem a)

-- Case 1
balanceL vt lt@(BinTree vlt@(RBItem Red vvlt) llt@(BinTree vllt@(RBItem Red vvllt) lllt rllt) rlt) rt   = BinTree vlt (make_black llt ) (BinTree vt rlt rt)
-- Case 2
balanceL vt lt@(BinTree vlt@(RBItem Red vvlt) llt rlt@(BinTree vrlt@(RBItem Red vvrlt) lrlt rrlt )) rt  = BinTree vt (BinTree vlt llt (make_black rlt)) rt
-- No violation
balanceL vt lt rt                                                                                       = BinTree vt lt rt
-- Case 3
balanceR vt lt rt@(BinTree vrt@(RBItem Red  vvrt) lrt rrt@(BinTree vrrt@(RBItem Red vvrrt) lrrt rrrt )) = BinTree vrt (BinTree vt lt lrt) (make_black rrt)
-- Case 4
balanceR vt lt rt@(BinTree vrt@(RBItem Red  vvrt) lrt@(BinTree vlrt@(RBItem Red vvlrt) llrt rlrt) rrt)  = BinTree vt lt (BinTree vrt (make_black lrt) rrt)
-- No violation
balanceR vt lt rt                                                                                       = BinTree vt lt rt

make_rbt :: Ord a => [ a ] -> RBT ( RBItem a )
make_rbt = foldl add_to_rbt EmptyBinTree

rbt_show h = map ( map rbt_show' ) (bin_tree_rows ( rbt_as_bin_tree h) )

rbt_show' EmptyBinTree = "E"
rbt_show' h            = show ( RBT.value ( BinTree.value h ) )

search_rbt :: Ord a => a -> RBT (RBItem a) -> RBT (RBItem a)
search_rbt v t = search_bst( RBItem Black v ) t




-- TESTING

balL :: Ord a => RBItem a -> RBT (RBItem a) -> RBT (RBItem a) -> Int
balR :: Ord a => RBItem a -> RBT (RBItem a) -> RBT (RBItem a) -> Int

-- Case 1
balL vt    lt@(BinTree vlt@(RBItem Red  vvlt) llt@(BinTree vllt@(RBItem Red   vvllt) lllt rllt) rlt) rt = 1
-- Case 2
balL vt    lt@(BinTree vlt@(RBItem Red  vvlt) llt rlt@(BinTree vrlt@(RBItem Red   vvrlt) lrlt rrlt )) rt = 2
-- No violation
balL vt    lt                                                                                         rt = 0
-- Case 3
balR vt lt rt@(BinTree vrt@(RBItem Red  vvrt) lrt rrt@(BinTree vrrt@(RBItem Red   vvrrt) lrrt rrrt ))    = 3
-- Case 4
balR vt lt rt@(BinTree vrt@(RBItem Red  vvrt) lrt@(BinTree vlrt@(RBItem Red   vvlrt) llrt rlrt) rrt)    = 4
-- No violation
balR vt lt rt                                                                                           = 0

case1 = let llt = BinTree (RBItem Red 5 ) EmptyBinTree EmptyBinTree
            lt  = BinTree (RBItem Red 10) llt          EmptyBinTree
            rt  = EmptyBinTree
            vt  = RBItem Black 20
        in balL vt lt rt

case2 = let rlt = BinTree (RBItem Red 10) EmptyBinTree EmptyBinTree
            lt  = BinTree (RBItem Red 0)  EmptyBinTree rlt
            rt  = EmptyBinTree
            vt  = RBItem Black 20
        in balL vt lt rt

case3 = let rrt = BinTree (RBItem Red 20) EmptyBinTree EmptyBinTree
            rt  = BinTree (RBItem Red 10) EmptyBinTree rrt
            lt  = EmptyBinTree
            vt  = RBItem Black 0
        in balR vt lt rt

case4 = let lrt = BinTree (RBItem Red 10) EmptyBinTree EmptyBinTree
            rt  = BinTree (RBItem Red 20) lrt          EmptyBinTree
            lt  = EmptyBinTree
            vt  = RBItem Black 0
        in balR vt lt rt
