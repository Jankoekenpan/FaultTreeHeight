package minimalcutpathset

import scala.collection.immutable.IntMap

def pathSetProbability(pathSet: PathSet, probabilities: IntMap[Probability]): Probability =
    pathSet.map(basicEvent => (1D - probabilities(basicEvent))).product

def height5(faultTree: FaultTree): Double = {
    val basicEvents = getBasicEvents(faultTree)
    val probabilities = getProbabilities(faultTree)(basicEvents)
    height5(faultTree, basicEvents, probabilities)
}

def height5(faultTree: FaultTree, basicEvents: Set[Event], probabilities: IntMap[Probability]): Double = {
    val pathSets = minimalPathSets(faultTree)(basicEvents)

    val (etas, height) = algorithm5(pathSets, probabilities)

    height
}

def algorithm5(pathSets: PathSets, basicEvents: IntMap[Probability]): (Etas, Double) = {
    val n = basicEvents.size
    val Pnil = pathSets

    val setFamiliesByPath = scala.collection.mutable.Map.empty[Path, PathSets]
    setFamiliesByPath.put(List(), Pnil)

    val maximumProductProbability = Pnil.map(cutSet => pathSetProbability(cutSet, basicEvents)).max
    val PnilStar: PathSets = for {
        P <- Pnil
        if doubleEqual(pathSetProbability(P, basicEvents), maximumProductProbability)
    } yield P

    val a: Event = PnilStar.flatten.maxBy(basicEvents)
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
                    val Px = setFamiliesByPath(x)
                    if j == Decision.Zero then
                        val Ps: PathSets = for P <- Px yield P - etas(x).asInstanceOf[Event]
                        setFamiliesByPath.put(s, Ps)

                        if Ps.contains(Set()) then
                            val etaS = j
                            val hS: 0 = 0
                            etas.put(s, etaS)
                            heights.put(s, hS)
                        else
                            val maxPs: Probability = Ps.map(p => pathSetProbability(p, basicEvents)).max
                            val PsStar = for {
                                P <- Ps
                                if doubleEqual(pathSetProbability(P, basicEvents), maxPs)
                            } yield P

                            val PS = PsStar.flatten

                            val b = PS.maxBy(basicEvents)
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
                        val Ps: PathSets = for P <- Px if !P.contains(etas(x).asInstanceOf[Event]) yield P
                        setFamiliesByPath.put(s, Ps)

                        if Ps.isEmpty then
                            val etaS = j
                            val hS: 0 = 0
                            etas.put(s, etaS)
                            heights.put(s, hS)
                        else
                            val maxPs: Probability = Ps.map(p => pathSetProbability(p, basicEvents)).max
                            val PsStar = for {
                                P <- Ps
                                if doubleEqual(pathSetProbability(P, basicEvents), maxPs)
                            } yield P

                            val PS = PsStar.flatten

                            val b = PS.maxBy(basicEvents)
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
def testAlgo5(): Unit = {
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
}
