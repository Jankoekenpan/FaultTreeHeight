package minimalcutpathset

import benchmark.Conversion

import scala.collection.immutable.IntMap

object MinceOrderedSet {


    def cutSetProbability(cutSet: CutSet, probabilities: IntMap[Probability]): Probability =
        cutSet.toSeq.map(basicEvent => probabilities(basicEvent)).product

    def getSmallestCutSetWithHighestProbability(cutSets: CutSets, probabilities: IntMap[Probability]): (CutSet, Probability) = {
        val cutSetsSeq = cutSets.toSeq;
        LazyList.from(cutSetsSeq)
            .zip(LazyList.from(cutSetsSeq).map(cs => cutSetProbability(cs, probabilities)))
            .sorted(
                Ordering.by[(CutSet, Probability), Int] { case (cs, prob) => cs.size }
                    .orElse(Ordering.by[(CutSet, Probability), Probability] { case (cs, prob) => prob }.reverse)
            ).head
    }

    def minceOrderedSet(faultTree: FaultTree, basicEvents: Set[Event], probabilities: IntMap[Probability]): Double = {
        val cutSets = minimalCutSets(faultTree)(basicEvents)

        val (etas, height) = minceOrderedSet(cutSets, probabilities)

        height
    }

    // paper: 'mince' or 'CuDA'.
    def minceOrderedSet(cutSets: CutSets, basicEvents: IntMap[Probability]): (Etas, Double) = {
        val n = basicEvents.size
        val Cnil = cutSets

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
                                val what: (CutSet, Probability) = getSmallestCutSetWithHighestProbability(Cs, basicEvents)
                                val CS: Set[Event] = what._1
//                                val maxCs: Probability = what._2

//                                val maxCs: Probability = Cs.map(c => cutSetProbability(c, basicEvents)).max
//                                val CsStar = for {
//                                    C <- Cs
//                                    if doubleEqual(cutSetProbability(C, basicEvents), maxCs)
//                                } yield C
//
//                                val CS: Set[Event] = CsStar.flatten

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
                                val what: (CutSet, Probability) = getSmallestCutSetWithHighestProbability(Cs, basicEvents)
                                val CS: Set[Event] = what._1
//                                val maxCs: Probability = what._2

//                                val maxCs: Probability = Cs.map(c => cutSetProbability(c, basicEvents)).max
//                                val CsStar = for {
//                                    C <- Cs
//                                    if doubleEqual(cutSetProbability(C, basicEvents), maxCs)
//                                } yield C
//
//                                val CS = CsStar.flatten

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

    def minceOrderedSet(faultTree: FaultTree): (Etas, Double) = {
        val basicEvents = getBasicEvents(faultTree)
        val probabilities = getProbabilities(faultTree)(basicEvents)
        val cutSets = minimalCutSets(faultTree)(basicEvents)
        //    println(cutSets)
        minceOrderedSet(cutSets, probabilities)
    }

    def main(args: Array[String]): Unit = {
        println(minceOrderedSet(Conversion.translateToDagTree(reallife.FT1_AssessingTheRisks2.FT)._1)._2)
    }

}
