module AVL
       where

import BinTree
import BST


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

type AVL a = BST a

avl_as_bin_tree :: AVL a -> BinTree a
avl_as_bin_tree t = t

avl_as_bst :: AVL a -> BST a
avl_as_bst t = t

avl_show :: Show a => AVL a -> [[[Char]]]
avl_show  = bin_tree_show  . avl_as_bin_tree
count_avl = count_bin_tree . avl_as_bin_tree

avl_depth :: BinTree (AVLItem a) -> Int
avl_depth EmptyBinTree = 0
avl_depth t@(BinTree (AVLItem dvt vvt) rt lt ) = dvt


rotate_avl_left :: BinTree (AVLItem a) -> BinTree (AVLItem a)
rotate_avl_left EmptyBinTree                                            = EmptyBinTree
rotate_avl_left t@( BinTree vt EmptyBinTree EmptyBinTree              ) = t
rotate_avl_left t@( BinTree vt lt           EmptyBinTree              ) = t

rotate_avl_left t@( BinTree (AVLItem dt vt) EmptyBinTree rt@( BinTree (AVLItem drt vrt) lrt rrt )) = let dlrt = avl_depth lrt
                                                                                                         drrt = avl_depth rrt
                                                                                                         nt   = (BinTree (AVLItem (1 + dlrt) vt ) EmptyBinTree lrt)
                                                                                                         dnt  = avl_depth nt
                                                                                                         nrt  = (BinTree (AVLItem (1 + max dnt drrt) vrt) nt rrt)
                                                                                                     in nrt

rotate_avl_left t@( BinTree (AVLItem dt vt) lt rt@( BinTree (AVLItem drt vrt) lrt rrt ) ) = let dlt  = avl_depth lt
                                                                                                dlrt = avl_depth lrt
                                                                                                drrt = avl_depth rrt
                                                                                                nt   = (BinTree (AVLItem (1 + max dlt dlrt) vt) lt lrt)
                                                                                                dnt  = avl_depth nt
                                                                                                nrt  = (BinTree (AVLItem (1 + max dnt drrt) vrt) nt rrt)
                                                                                            in nrt




rotate_avl_right :: BinTree (AVLItem a) -> BinTree (AVLItem a)
rotate_avl_right EmptyBinTree                                                           = EmptyBinTree
rotate_avl_right t@( BinTree vt EmptyBinTree               EmptyBinTree               ) = t
rotate_avl_right t@( BinTree vt EmptyBinTree               rt                         ) = t

rotate_avl_right t@( BinTree (AVLItem dt vt) lt@( BinTree (AVLItem dlt vlt) llt rlt ) EmptyBinTree ) = let dllt = avl_depth llt
                                                                                                           drlt = avl_depth rlt
                                                                                                           dnt  = 1 + drlt
                                                                                                           dnlt = 1 + max dllt dnt
                                                                                                           nt   = (BinTree (AVLItem dt vt) rlt EmptyBinTree)
                                                                                                           nlt  = (BinTree (AVLItem dnlt vlt) llt nt)
                                                                                                       in nlt

rotate_avl_right t@( BinTree (AVLItem dt vt) lt@( BinTree (AVLItem dlt vlt) llt rlt ) rt) = let dllt = avl_depth llt
                                                                                                drlt = avl_depth rlt
                                                                                                drt  = avl_depth rt
                                                                                                dnt  = 1 + max drlt drt
                                                                                                dnlt = 1 + max dllt dnt
                                                                                                nt   = (BinTree (AVLItem dnt vt) rlt rt)
                                                                                                nlt  = (BinTree (AVLItem dnlt vlt) llt nt)
                                                                                            in nlt
                                                                                                


add_to_avl :: Ord a => AVL (AVLItem a) -> a -> AVL (AVLItem a)

add_to_avl EmptyBinTree          nv             = BinTree (AVLItem 1 nv) EmptyBinTree EmptyBinTree

add_to_avl t@( BinTree (AVLItem dt vt) lt rt) nv | nv > vt   = let nrt  = add_to_avl rt nv
                                                                   dnrt = avl_depth nrt
                                                                   dlt  = avl_depth lt
                                                                   d    = max dnrt dlt
                                                                   nt   = (BinTree (AVLItem (d+1) vt) lt nrt)
                                                               in if dnrt - dlt >= 2
                                                                  then  rotate_avl_left nt
                                                                  else  nt
                                                 | otherwise = let nlt  = add_to_avl lt nv
                                                                   dnlt = avl_depth nlt
                                                                   drt  = avl_depth rt
                                                                   d    = max dnlt drt
                                                                   nt   = (BinTree (AVLItem (d+1) vt) nlt rt)
                                                               in if dnlt - drt >= 2
                                                                  then rotate_avl_right nt
                                                                  else nt

make_avl :: Ord a => [ a ] -> AVL (AVLItem a)
make_avl = foldl add_to_avl EmptyBinTree

search_avl v EmptyBinTree = EmptyBinTree
search_avl v t@( BinTree vt lt rt ) | v >  vt   = search_avl v rt
                                    | v == vt   = t
                                    | otherwise = search_avl v lt

merge_avl EmptyBinTree         EmptyBinTree                      = EmptyBinTree
merge_avl EmptyBinTree         r                                 = r
merge_avl l                    EmptyBinTree                      = l
merge_avl l@(BinTree vl ll rl) r@(BinTree vr lr rr ) | vl > vr   = BinTree vl ll               (merge_avl rl r)
                                                     | otherwise = BinTree vr (merge_avl l lr) rr

delete_avl EmptyBinTree      ov             = EmptyBinTree
delete_avl t@(BinTree v l r) ov | ov > v    = BinTree v l (delete_avl r ov)
                                | ov == v   = merge_avl l r
                                | otherwise = BinTree v (delete_avl l ov) r
