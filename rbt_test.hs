import Test.HUnit
import System.Random
import RBT
import BinTree

test1 = TestCase (let rbt = make_rbt [1..200]
            depth = bin_tree_depth ( rbt_as_bin_tree rbt )
            minDepth = logBase 2 (200+1)
            maxDepth = 2 * logBase 2 (200+1)
        in assert ( depth <= maxDepth && depth > minDepth ) )
            

tests = TestList [ TestLabel "test1" test1 ]

