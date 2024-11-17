package minimalcutpathset

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class FaultTreeTest {

    @Test
    def testFlatten(): Unit = {
        val original = FaultTree(16, Map(
            16 -> TreeNode.Combination(16, Gate.And, Set(0, 14, 10, 13, 15, 12, 4)),
            15 -> TreeNode.Combination(15, Gate.And, Set(13, 10, 2, 11, 12, 9, 6, 8, 3, 5, 4)),
            14 -> TreeNode.Combination(14, Gate.And, Set(0, 9, 13, 8, 3, 5)),
            13 -> TreeNode.Combination(13, Gate.Or, Set(1, 7, 10, 2, 11, 4)),
            12 -> TreeNode.Combination(12, Gate.Or, Set(10, 6, 11, 8, 2, 5, 4)),
            11 -> TreeNode.Combination(11, Gate.Or, Set(0, 6, 7, 2, 8, 3, 4)),
            10 -> TreeNode.Combination(10, Gate.Or, Set(0, 9, 1, 6, 7, 2, 8, 3, 5, 4)),
            9 -> TreeNode.BasicEvent(9, 0),
            8 -> TreeNode.BasicEvent(8, 0),
            7 -> TreeNode.BasicEvent(7, 0),
            6 -> TreeNode.BasicEvent(6, 0),
            5 -> TreeNode.BasicEvent(5, 0),
            4 -> TreeNode.BasicEvent(4, 0),
            3 -> TreeNode.BasicEvent(3, 0),
            2 -> TreeNode.BasicEvent(2, 0),
            1 -> TreeNode.BasicEvent(1, 0),
            0 -> TreeNode.BasicEvent(0, 0)
        ))

        val expected = FaultTree(16, Map(
            16 -> TreeNode.Combination(16, Gate.And, Set(0, 9, 13, 8, 3, 5, 10, 2, 11, 12, 6, 4)),
            //15 is removed
            //14 is removed
            13 -> TreeNode.Combination(13, Gate.Or, Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
            12 -> TreeNode.Combination(12, Gate.Or, Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)),
            11 -> TreeNode.Combination(11, Gate.Or, Set(0, 6, 7, 2, 8, 3, 4)),
            10 -> TreeNode.Combination(10, Gate.Or, Set(0, 9, 1, 6, 7, 2, 8, 3, 5, 4)),
            9 -> TreeNode.BasicEvent(9, 0),
            8 -> TreeNode.BasicEvent(8, 0),
            7 -> TreeNode.BasicEvent(7, 0),
            6 -> TreeNode.BasicEvent(6, 0),
            5 -> TreeNode.BasicEvent(5, 0),
            4 -> TreeNode.BasicEvent(4, 0),
            3 -> TreeNode.BasicEvent(3, 0),
            2 -> TreeNode.BasicEvent(2, 0),
            1 -> TreeNode.BasicEvent(1, 0),
            0 -> TreeNode.BasicEvent(0, 0)
        ))

        assertEquals(expected, flatten(original))
    }

}
