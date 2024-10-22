package minimalcutpathset

import scala.collection.immutable.IntMap

def doubleEqual(one: Double, two: Double): Boolean =
    Math.abs(one - two) < 0.00_000_1

def cutSetProbability(cutSet: CutSet, probabilities: IntMap[Probability]): Probability =
    cutSet.map(basicEvent => probabilities(basicEvent)).product

def implies(lhs: Boolean, rhs: Boolean): Boolean =
    !lhs || rhs

def algorithm6(cutSets: CutSets, basicEvents: IntMap[Probability]): (Etas, Double) = {

    val minimumProductProbability = cutSets.map(cutSet => cutSetProbability(cutSet, basicEvents)).min

    val Cstar: CutSets = for {
        C <- cutSets
        if doubleEqual(cutSetProbability(C, basicEvents), minimumProductProbability)
    } yield C

    val CstarUnion = Cstar.flatten

    val a = CstarUnion.minBy(event => basicEvents(event))
    val probA = basicEvents(a)

    val etas: Etas = scala.collection.mutable.Map.empty
    etas.update(List(), a)

    val heights: Heights = scala.collection.mutable.Map.empty
    heights.put(List(), new CachedFunction0[Probability](() => 1 +
        (1 - probA) * asDouble(heights(List(Decision.Zero))) +
        probA * asDouble(heights(List(Decision.One)))))

    val X = scala.collection.mutable.Map.empty[Int, Set[Path]].withDefaultValue(Set())
    X.put(0, Set(List()))

    for (i <- 0 to (basicEvents.size - 1)) {
        val Xi = X(i)
        if (Xi.nonEmpty) {
            for (x <- Xi) {
                for (j <- Seq(Decision.One, Decision.Zero)) {
                    val s: Path = j :: x
                    val Cs: CutSets = (for {
                        t: Path <- s.tails.drop(1)
                        etaT: Eta = etas(t)
                        if etaT.isInstanceOf[Event]
                        etaTEvent = etaT.asInstanceOf[Event]
                        C: CutSet <- cutSets
                        if implies(s.endsWith(Decision.One :: t), C.contains(etaTEvent))
                        if implies(s.endsWith(Decision.Zero :: t), !C.contains(etaTEvent))
                    } yield C).toSet

                    val minProductProbability: Probability = Cs.map(Cprime => cutSetProbability(Cprime, basicEvents)).min
                    val CsStar = for {
                        C <- Cs
                        if doubleEqual(cutSetProbability(C, basicEvents), minProductProbability)
                    } yield C

                    val cSprime: Set[Event] = (for {
                        t: Path <- x.tails
                        event: Eta <- etas.get(t)
                    } yield event.asInstanceOf[Event]).toSet

                    val cS: Set[Event] = CsStar.flatten -- cSprime

                    if (cS.nonEmpty) {
                        val b = cS.minBy(basicEvents)
                        etas.put(s, b)
                        val probB = basicEvents(b)
                        heights.put(s, new CachedFunction0[Probability](() => 1D +
                            (1D - probB) * asDouble(heights(Decision.Zero :: s)) +
                            probB * asDouble(heights(Decision.One :: s))))
                        X.updateWith(i + 1) {
                            case Some(paths) => Some(paths + s)
                            case None => Some(Set(s))
                        }
                    } else {
                        etas.put(s, j)
                        heights.put(s, 0: 0)
                    }
                }
            }
        }
    }

    (etas, asDouble(heights(List())))
}

@main
def testAlgo6(): Unit = {
    val (etas, height) = algorithm6(
        Set(Set(0, 1), Set(1, 2), Set(2, 0)),
        IntMap(0 -> 1D/4D, 1 -> 1D/2D, 2 -> 1D/3D)
    )
    println(etas)
    println(height)
}