package faulttree

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertEquals

class FaultTreeTest {

    @Test
    def testFlattenUnchanged(): Unit = {
        val originalAnd = FaultTree.AndEvent(0, Seq(
            FaultTree.OrEvent(1, Seq(
                FaultTree.AndEvent(2, Seq(
                    FaultTree.BasicEvent(3, 0.0),
                    FaultTree.BasicEvent(4, 0.0)
                )),
                FaultTree.BasicEvent(5, 0.0)
            )),
            FaultTree.BasicEvent(6, 0.0)
        ))
        val flattenedAnd = flatten(originalAnd)
        assertEquals(originalAnd, flattenedAnd)

        val originalOr = FaultTree.OrEvent(0, Seq(
            FaultTree.AndEvent(1, Seq(
                FaultTree.OrEvent(2, Seq(
                    FaultTree.BasicEvent(3, 0.0),
                    FaultTree.BasicEvent(4, 0.0)
                )),
                FaultTree.BasicEvent(5, 0.0)
            )),
            FaultTree.BasicEvent(6, 0.0)
        ))
        val flattenedOr = flatten(originalOr)
        assertEquals(originalOr, flattenedOr)
    }

    @Test
    def testFlattenAnd(): Unit = {
        val tree = FaultTree.AndEvent(0, Seq(
            FaultTree.OrEvent(1, Seq(
                FaultTree.BasicEvent(2, 0.0),
                FaultTree.AndEvent(3, Seq(
                    FaultTree.BasicEvent(12, 0.0),
                    FaultTree.BasicEvent(13, 0.0)
                ))
            )),
            FaultTree.AndEvent(4, Seq(
                FaultTree.BasicEvent(5, 0.0),
                FaultTree.AndEvent(6, Seq(
                    FaultTree.BasicEvent(7, 0.0),
                    FaultTree.BasicEvent(8, 0.0)
                )),
                FaultTree.OrEvent(9, Seq(
                    FaultTree.BasicEvent(10, 0.0),
                    FaultTree.BasicEvent(11, 0.0)
                ))
            ))
        ))

        val expected = FaultTree.AndEvent(0, Seq(
            FaultTree.OrEvent(1, Seq(
                FaultTree.BasicEvent(2,0.0),
                FaultTree.AndEvent(3, Seq(
                    FaultTree.BasicEvent(12,0.0),
                    FaultTree.BasicEvent(13,0.0)
                ))
            )),
            FaultTree.BasicEvent(5,0.0),
            FaultTree.BasicEvent(7,0.0),
            FaultTree.BasicEvent(8,0.0),
            FaultTree.OrEvent(9, Seq(
                FaultTree.BasicEvent(10,0.0),
                FaultTree.BasicEvent(11,0.0)
            ))
        ))

        assertEquals(expected, flatten(tree))
    }

    @Test
    def testFlattenOr(): Unit = {
        val tree = FaultTree.OrEvent(0, Seq(
            FaultTree.AndEvent(1, Seq(
                FaultTree.BasicEvent(2, 0.0),
                FaultTree.OrEvent(3, Seq(
                    FaultTree.BasicEvent(12, 0.0),
                    FaultTree.BasicEvent(13, 0.0)
                ))
            )),
            FaultTree.OrEvent(4, Seq(
                FaultTree.BasicEvent(5, 0.0),
                FaultTree.OrEvent(6, Seq(
                    FaultTree.BasicEvent(7, 0.0),
                    FaultTree.BasicEvent(8, 0.0)
                )),
                FaultTree.AndEvent(9, Seq(
                    FaultTree.BasicEvent(10, 0.0),
                    FaultTree.BasicEvent(11, 0.0)
                ))
            ))
        ))

        val expected = FaultTree.OrEvent(0, Seq(
            FaultTree.AndEvent(1, Seq(
                FaultTree.BasicEvent(2, 0.0),
                FaultTree.OrEvent(3, Seq(
                    FaultTree.BasicEvent(12, 0.0),
                    FaultTree.BasicEvent(13, 0.0)
                ))
            )),
            FaultTree.BasicEvent(5, 0.0),
            FaultTree.BasicEvent(7, 0.0),
            FaultTree.BasicEvent(8, 0.0),
            FaultTree.AndEvent(9, Seq(
                FaultTree.BasicEvent(10, 0.0),
                FaultTree.BasicEvent(11, 0.0)
            ))
        ))

        assertEquals(expected, flatten(tree))
    }
}
