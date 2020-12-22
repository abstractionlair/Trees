module AVL
       where

-- AVL (Adelson, Velsky, Landis) Trees

import BinTree


data AVLItem a = AVLItem { depth :: Int, value :: a } deriving Show

instance (Eq a) => Eq (AVLItem a) where
         (==)    (AVLItem c1 v1)    (AVLItem c2 v2)             = v1 == v2
         (/=)    (AVLItem c1 v1)    (AVLItem c2 v2)             = v1 /= v2

instance (Ord a) => Ord (AVLItem a) where
         (<)     (AVLItem c1 v1)    (AVLItem c2 v2)             = v1 <  v2
         (<=)    (AVLItem c1 v1)    (AVLItem c2 v2)             = v1 <= v2
         (>=)    (AVLItem c1 v1)    (AVLItem c2 v2)             = v1 >= v2
         (>)     (AVLItem c1 v1)    (AVLItem c2 v2)             = v1 >  v2
         max  p1@(AVLItem c1 v1) p2@(AVLItem c2 v2) | v1 >  v2  = p1
                                                    | otherwise = p2
         min  p1@(AVLItem c1 v1) p2@(AVLItem c2 v2) | v1 < v2   = p1
                                                    | otherwise = p2

type AVL a = BinTree a

avl_depth :: BinTree (AVLItem a) -> Int
avl_depth EmptyBinTree = 0
avl_depth (BinTree (AVLItem dvt vvt) rt lt) = dvt

avl_rotate_left :: BinTree (AVLItem a) -> BinTree (AVLItem a)
avl_rotate_left EmptyBinTree                                                                  = EmptyBinTree
avl_rotate_left t@(BinTree vt              EmptyBinTree EmptyBinTree                        ) = t
avl_rotate_left t@(BinTree vt              lt           EmptyBinTree                        ) = t
avl_rotate_left   (BinTree (AVLItem dt vt) lt           (BinTree (AVLItem drt vrt) lrt rrt )) = let dlt  = avl_depth lt
                                                                                                    dlrt = avl_depth lrt
                                                                                                    drrt = avl_depth rrt
                                                                                                    dnt  = 1 + max dlt dlrt
                                                                                                    dnrt = 1 + max dnt drrt
                                                                                                    nt   = (BinTree (AVLItem dnt  vt ) lt lrt)
                                                                                                    nrt  = (BinTree (AVLItem dnrt vrt) nt rrt)
                                                                                                in nrt

avl_rotate_right :: BinTree (AVLItem a) -> BinTree (AVLItem a)
avl_rotate_right EmptyBinTree                                                                  = EmptyBinTree
avl_rotate_right t@(BinTree vt              EmptyBinTree                         EmptyBinTree) = t
avl_rotate_right t@(BinTree vt              EmptyBinTree                         rt          ) = t
avl_rotate_right   (BinTree (AVLItem dt vt) (BinTree (AVLItem dlt vlt) llt rlt ) rt          ) = let dllt = avl_depth llt
                                                                                                     drlt = avl_depth rlt
                                                                                                     drt  = avl_depth rt
                                                                                                     dnt  = 1 + max drlt drt
                                                                                                     dnlt = 1 + max dllt dnt
                                                                                                     nt   = (BinTree (AVLItem dnt  vt ) rlt rt)
                                                                                                     nlt  = (BinTree (AVLItem dnlt vlt) llt nt)
                                                                                                 in nlt
                                                                                                


avl_add :: Ord a => AVL (AVLItem a) -> a -> AVL (AVLItem a)
avl_add EmptyBinTree                      nv             = BinTree (AVLItem 1 nv) EmptyBinTree EmptyBinTree
avl_add t@(BinTree (AVLItem dt vt) lt rt) nv | nv > vt   = let nrt  = avl_add rt nv
                                                               dnrt = avl_depth nrt
                                                               dlt  = avl_depth lt
                                                               dnt  = 1 + max dnrt dlt
                                                               nt   = (BinTree (AVLItem dnt vt) lt nrt)
                                                           in if dnrt - dlt >= 2
                                                              then  avl_rotate_left nt
                                                              else  nt
                                             | nv == vt  = t
                                             | otherwise = let nlt  = avl_add lt nv
                                                               dnlt = avl_depth nlt
                                                               drt  = avl_depth rt
                                                               dnt  = 1 + max dnlt drt
                                                               nt   = (BinTree (AVLItem dnt vt) nlt rt)
                                                           in if dnlt - drt >= 2
                                                              then avl_rotate_right nt
                                                              else nt


avl_make :: Ord a => [ a ] -> AVL (AVLItem a)
avl_make = foldl avl_add EmptyBinTree


avl_search :: Ord a => a -> BinTree (AVLItem a) -> BinTree (AVLItem a)
avl_search v EmptyBinTree = EmptyBinTree
avl_search v t@(BinTree (AVLItem dt vt) lt rt) | v >  vt   = avl_search v rt
                                               | v == vt   = t
                                               | otherwise = avl_search v lt


avl_delete_merge EmptyBinTree                      EmptyBinTree                                   = EmptyBinTree
avl_delete_merge EmptyBinTree                      r                                              = r
avl_delete_merge l                                 EmptyBinTree                                   = l

--Not a general purpose merge
avl_delete_merge l@(BinTree vl@(AVLItem dl vvl) ll rl) r@(BinTree vr@(AVLItem dr vvr) lr rr ) | dl > dr   = let nr  = avl_delete_merge rl r
                                                                                                                dnr = avl_depth nr
                                                                                                                dll = avl_depth ll
                                                                                                                dnt = 1 + max dll dnr
                                                                                                                nt  = BinTree (AVLItem dnt vvl) ll nr
                                                                                                                bal = dll - dnr
                                                                                                            in if bal >= 2
                                                                                                               then avl_rotate_right nt
                                                                                                               else if bal <= 2
                                                                                                                    then avl_rotate_left nt
                                                                                                                    else nt
                                                                                              | otherwise = let nl  = avl_delete_merge l lr
                                                                                                                dnl = avl_depth nl
                                                                                                                drr = avl_depth rr
                                                                                                                dnt = 1 + max dnl drr
                                                                                                                nt  = BinTree (AVLItem dnt vvr) nl rr
                                                                                                                bal = dnl - drr
                                                                                                            in if bal >= 2
                                                                                                               then avl_rotate_right nt
                                                                                                               else if bal <= 2
                                                                                                                    then avl_rotate_left nt
                                                                                                                    else nt


avl_delete :: Ord a => AVL (AVLItem a) -> a -> AVL (AVLItem a)
avl_delete EmptyBinTree                         ov                = EmptyBinTree
avl_delete (BinTree v@(AVLItem dv vv) l r) ov | ov > vv = let nr  = avl_delete r ov
                                                              dnr = avl_depth nr
                                                              dl  = avl_depth l
                                                              dnt = 1 + max dl dnr
                                                              nt  = BinTree (AVLItem dnt vv) l nr
                                                          in if dl - dnr >= 2
                                                             then avl_rotate_right nt
                                                             else nt
                                           | ov == vv   = avl_delete_merge l r
                                           | otherwise  = let nl  = avl_delete l ov
                                                              dnl = avl_depth nl
                                                              dr  = avl_depth r
                                                              dnt = 1 + max dnl dr
                                                              nt  = BinTree (AVLItem dnt vv) nl r
                                                          in if dr - dnl >= 2
                                                             then avl_rotate_left nt
                                                             else nt











avl_as_bin_tree :: AVL a -> BinTree a
avl_as_bin_tree t = t

avl_count = bin_tree_count . avl_as_bin_tree

avl_show :: Show a => AVL (AVLItem a) -> [[[Char]]]
avl_show h = map ( map avl_show' ) (bin_tree_rows (avl_as_bin_tree h))

avl_show' :: Show a => BinTree (AVLItem a) -> [Char]
avl_show' EmptyBinTree = "E"
avl_show' h            = show ( AVL.value ( BinTree.value h ) )

avl_show_debug :: Show a => AVL (AVLItem a) -> [[[Char]]]
avl_show_debug h = map ( map avl_show_debug' ) (bin_tree_rows (avl_as_bin_tree h))

avl_show_debug' :: Show a => BinTree (AVLItem a) -> [Char]
avl_show_debug' EmptyBinTree = "E"
avl_show_debug' h            = show ( BinTree.value h )

avl_show_debug2 :: Show a => AVL (AVLItem a) -> [[[Char]]]
avl_show_debug2 h = map ( map avl_show_debug2' ) (bin_tree_rows (avl_as_bin_tree h))

avl_show_debug2' :: Show a => BinTree (AVLItem a) -> [Char]
avl_show_debug2' EmptyBinTree = "E"
avl_show_debug2' h            = show ( AVL.value (BinTree.value h ), AVL.depth (BinTree.value h ) )

avl_elem v t = not_empty $ avl_search v t
               where not_empty EmptyBinTree = False
                     not_empty _            = True

















-- Not a general purpose balance
-- balance t@(BinTree vt lt rt ) = let dlt = avl_depth lt
--                                     drt = avl_depth rt
--                                 in if dlt - drt >= 2
--                                    then balance (avl_rotate_right t)
--                                    else if drt - dlt >= 2
--                                         then balance (avl_rotate_left t)
--                                         else t


--avl_rotate_left ( BinTree (AVLItem dt vt) EmptyBinTree rt@( BinTree (AVLItem drt vrt) lrt rrt )) = let dlrt = avl_depth lrt
--                                                                                                       drrt = avl_depth rrt
--                                                                                                       nt   = (BinTree (AVLItem (1 + dlrt) vt ) EmptyBinTree lrt)
--                                                                                                       dnt  = avl_depth nt
--                                                                                                       nrt  = (BinTree (AVLItem (1 + max dnt drrt) vrt) nt rrt)
--                                                                                                   in nrt


-- avl_rotate_right t@( BinTree (AVLItem dt vt) lt@( BinTree (AVLItem dlt vlt) llt rlt ) EmptyBinTree ) = let dllt = avl_depth llt
--                                                                                                            drlt = avl_depth rlt
--                                                                                                            dnt  = 1 + drlt
--                                                                                                            dnlt = 1 + max dllt dnt
--                                                                                                            nt   = (BinTree (AVLItem dt vt) rlt EmptyBinTree)
--                                                                                                            nlt  = (BinTree (AVLItem dnlt vlt) llt nt)
--                                                                                                        in nlt

-- merge_avl l@(BinTree vl@(AVLItem dl vvl) ll rl) r@(BinTree vr@(AVLItem dr vvr) lr rr ) | vvl > vvr = let nr  = merge_avl rl r
--                                                                                                          dnr = avl_depth nr
--                                                                                                          dll = avl_depth ll
--                                                                                                          dnt = 1 + max dll dnr
--                                                                                                          nt  = BinTree (AVLItem dnt vvl) ll nr
--                                                                                                      in nt --balance nt
--                                                                                        | otherwise = let nl  = merge_avl l lr
--                                                                                                          dnl = avl_depth nl
--                                                                                                          drr = avl_depth rr
--                                                                                                          dnt = 1 + max dnl drr
--                                                                                                          nt  = BinTree (AVLItem dnt vvr) nl rr
--                                                                                                      in nt -- balance nt

