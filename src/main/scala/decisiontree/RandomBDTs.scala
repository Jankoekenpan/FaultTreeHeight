package decisiontree

import java.util.random.RandomGenerator
import scala.collection.immutable.IntMap

object RandomBDTs {
    import BooleanFormula.*

    def getBasicEvents(formula: BooleanFormula): Set[Event] = formula match {
        case True | False => Set()
        case Variable(id) => Set(id)
        case And(lhs, rhs) => getBasicEvents(lhs) union getBasicEvents(rhs)
        case Or(lhs, rhs) => getBasicEvents(lhs) union getBasicEvents(rhs)
    }

    def randomChoose(events: Set[Event])(using random: RandomGenerator): Event = {
        val seq = events.toIndexedSeq
        seq(random.nextInt(seq.size))
    }

    @java.lang.Deprecated // no longer used in paper.
    def height(events: Set[Event], formula: BooleanFormula, probabilities: IntMap[Probability])(using random: RandomGenerator): (Double, BinaryDecisionTree) = formula match {
        case True => (0D, BinaryDecisionTree.One)
        case False => (0D, BinaryDecisionTree.Zero)
        case _ =>
            val b = randomChoose(events)
            val pb = probabilities(b)

            val ftb1 = subsuper(formula, b, true)
            val ftb0 = subsuper(formula, b, false)

            val recEvents = events - b

            val (heightFTb1, bdtB1) = height(recEvents, ftb1, probabilities)
            val (heightFTb0, bdtB0) = height(recEvents, ftb0, probabilities)

            val hb = 1 + pb * heightFTb1 + (1 - pb) * heightFTb0
            val bdtB = BinaryDecisionTree.NonLeaf(b, bdtB0, bdtB1)

            (hb, bdtB)
    }

    def algorithm13(formula: BooleanFormula, probabilities: IntMap[Probability])(using random: RandomGenerator): Double = {
        val basicEvents = getBasicEvents(formula)

        val (h, bdt) = height(basicEvents, formula, probabilities)

        h
    }

    def main(args: Array[String]): Unit = {
        given random: RandomGenerator = new java.util.Random()

        val formula = Or(And(Variable(2), Or(Variable(0), Variable(1))), And(Variable(0), Variable(1)))
        val probabilities = IntMap(0 -> 1D/2D, 1 -> 1D/2D, 2 -> 1D/2D)
        val events = getBasicEvents(formula)

        val (h, bdt) = height(events, formula, probabilities)

        println(h)
        println(bdt)
    }

}
