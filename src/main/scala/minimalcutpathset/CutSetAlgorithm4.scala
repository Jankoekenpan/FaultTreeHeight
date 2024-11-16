package minimalcutpathset

import scala.collection.immutable.IntMap

def height4(faultTree: FaultTree): Double = {
    val basicEvents = getBasicEvents(faultTree)
    val probabilities = getProbabilities(faultTree)(basicEvents)
    height4(faultTree, basicEvents, probabilities)
}

def height4(faultTree: FaultTree, basicEvents: Set[Event], probabilities: IntMap[Probability]): Double = {
    val cutSets = minimalCutSets(faultTree)(basicEvents)

    val (etas, height) = algorithm4(cutSets, probabilities)

    height
}

def algorithm4(cutSets: CutSets, basicEvents: IntMap[Probability]): (Etas, Double) = {
    val n = basicEvents.size
    val Cnil = cutSets

    //println(s"DEBUG: Cnil = ${Cnil}") //TODO remove

    val setFamiliesByPath = scala.collection.mutable.Map.empty[Path, CutSets]
    setFamiliesByPath.put(List(), Cnil)

    val maximumProductProbability = Cnil.map(cutSet => cutSetProbability(cutSet, basicEvents)).max
    val CnilStar: CutSets = for {
        C <- Cnil
        if doubleEqual(cutSetProbability(C, basicEvents), maximumProductProbability)
    } yield C

    val a: Event = CnilStar.flatten.minBy(basicEvents)
    val probA = basicEvents(a)

    val etas: Etas = scala.collection.mutable.Map.empty
    etas.update(List(), a)

    val heights: Heights = scala.collection.mutable.Map.empty
    heights.put(List(), new CachedFunction0[Probability](() => 1 +
        (1 - probA) * asDouble(heights(List(Decision.Zero))) +
        probA * asDouble(heights(List(Decision.One)))))

    val X = scala.collection.mutable.Map.empty[Int, Set[Path]].withDefaultValue(Set())
    X.put(0, Set(List()))

    for i <- 0 to n - 1 do
        val Xi = X(i)
        if Xi.nonEmpty then
            for x <- Xi do
                for j <- Seq(Decision.One, Decision.Zero) do
                    val s = j :: x
                    val Cx = setFamiliesByPath(x)
                    if j == Decision.One then
                        val Cs: CutSets = for C <- Cx yield C - etas(x).asInstanceOf[Event]
                        setFamiliesByPath.put(s, Cs)

                        if Cs.contains(Set()) then
                            val etaS = j
                            val hS: 0 = 0
                            etas.put(s, etaS)
                            heights.put(s, hS)
                        else
                            val maxCs: Probability = Cs.map(c => cutSetProbability(c, basicEvents)).max
                            val CsStar = for {
                                C <- Cs
                                if doubleEqual(cutSetProbability(C, basicEvents), maxCs)
                            } yield C

                            val CS = CsStar.flatten

                            val b = CS.minBy(basicEvents)
                            val probB = basicEvents(b)

                            val etaS = b
                            val hS = new CachedFunction0[Probability](() => 1 +
                                (1 - probB) * asDouble(heights(Decision.Zero :: s)) +
                                probB * asDouble(heights(Decision.One :: s)))

                            etas.put(s, etaS)
                            heights.put(s, hS)

                            X.updateWith(i + 1) {
                                case Some(paths) => Some(paths + s)
                                case None => Some(Set(s))
                            }
                        end if
                    else
                        val Cs: CutSets = for C <- Cx if !C.contains(etas(x).asInstanceOf[Event]) yield C
                        setFamiliesByPath.put(s, Cs)

                        if Cs.isEmpty then
                            val etaS = j
                            val hS: 0 = 0
                            etas.put(s, etaS)
                            heights.put(s, hS)
                        else
                            val maxCs: Probability = Cs.map(c => cutSetProbability(c, basicEvents)).max
                            val CsStar = for {
                                C <- Cs
                                if doubleEqual(cutSetProbability(C, basicEvents), maxCs)
                            } yield C

                            val CS = CsStar.flatten

                            val b = CS.minBy(basicEvents)
                            val probB = basicEvents(b)

                            val etaS = b
                            val hS = new CachedFunction0[Probability](() => 1 +
                                (1 - probB) * asDouble(heights(Decision.Zero :: s)) +
                                probB * asDouble(heights(Decision.One :: s)))

                            etas.put(s, etaS)
                            heights.put(s, hS)

                            X.updateWith(i + 1) {
                                case Some(paths) => Some(paths + s)
                                case None => Some(Set(s))
                            }
                        end if
                    end if
                end for
            end for
        end if
    end for

    (etas, asDouble(heights(List())))
}

@main
def testAlgo4(): Unit = {
    println(height4(
        FaultTree(1, Map(
            1 -> TreeNode.Combination(1, Gate.Or, Set(2, 3)),
            2 -> TreeNode.Combination(2, Gate.And, Set(4, 5, 6)),
            3 -> TreeNode.Combination(3, Gate.And, Set(7, 8)),
            4 -> TreeNode.BasicEvent(4, 0.5),
            5 -> TreeNode.BasicEvent(5, 0.7),
            6 -> TreeNode.BasicEvent(6, 0.4),
            7 -> TreeNode.BasicEvent(7, 0.6),
            8 -> TreeNode.BasicEvent(8, 0.2)
        ))
    ))  // expected: 2.632

    println(height4(reallife.T0Chopper.FT)) // expected: 15.877624503409331
}
