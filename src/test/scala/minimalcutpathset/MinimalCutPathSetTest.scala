package minimalcutpathset

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class MinimalCutPathSetTest {

    @Test
    def testRemoveSupersets(): Unit = {
        val sets = Seq(Set(1), Set(2), Set(1, 2), Set(3, 4), Set(3), Set(4))

        val actual = removeSupersets(sets)

        val expected = Seq(Set(1), Set(2), Set(3), Set(4))

        assertEquals(expected, actual)
    }

}
