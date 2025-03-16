package faulttree

import scala.collection.mutable

// paper: 'remind' (for tree-like fault trees).
def height7(tree: FaultTree): Real = {
    height7(tree, layers(tree))
}

def height7(tree: FaultTree, layers: IArray[Seq[FaultTree]]): Real = {
    inline def eventsAtLayer(layer: Int): Seq[FaultTree] = layers(layer)
    inline def basicEvents: Seq[FaultTree.BasicEvent] = eventsAtLayer(0).asInstanceOf[Seq[FaultTree.BasicEvent]]

    val height0Map: mutable.Map[Event, Seq[Int]] = new mutable.HashMap()
    val height1Map: mutable.Map[Event, Seq[Int]] = new mutable.HashMap()

    val probabilities0Map: mutable.Map[Event, Seq[Probability]] = mutable.HashMap()
    val probabilities1Map: mutable.Map[Event, Seq[Probability]] = mutable.HashMap()

    def P(eventU: FaultTree): Probability = probabilities1Map(eventU.event).sum

    for (x <- basicEvents) {
        height0Map.put(x.event, Seq(1))
        height1Map.put(x.event, Seq(1))
        probabilities0Map.put(x.event, Seq(1 - x.probability))
        probabilities1Map.put(x.event, Seq(x.probability))
    }

    val topLayer = layers.size - 1
    for (j <- 1 to topLayer) {
        for (x <- eventsAtLayer(j)) {
            x match {
                case FaultTree.AndEvent(event, children) =>
                    val rank: Seq[FaultTree] = children.sortBy(P)

                    val x0Height: Seq[Int] = nonEmptyInits(rank).flatMap {
                        case init :+ last => init.foldRight(height0Map(last.event))((event, resHeight) => bigoplus(height1Map(event.event), resHeight))
                    }
                    height0Map.put(x.event, x0Height)

                    val x0Probability: Seq[Probability] = nonEmptyInits(rank).flatMap {
                        case init :+ last => init.foldRight(probabilities0Map(last.event))((event, resProb) => bigotimes(probabilities1Map(event.event), resProb))
                    }
                    probabilities0Map.put(x.event, x0Probability)

                    val x1Height: Seq[Int] = rank.map(event => height1Map(event.event)).reduce(bigoplus)
                    height1Map.put(x.event, x1Height)

                    val x1Probability: Seq[Probability] = rank.map(event => probabilities1Map(event.event)).reduce(bigotimes)
                    probabilities1Map.put(x.event, x1Probability)

                case FaultTree.OrEvent(event, children) =>
                    val rank: Seq[FaultTree] = children.sortBy(P)(using summon[Ordering[Probability]].reverse)

                    val x1Height: Seq[Int] = nonEmptyInits(rank).flatMap {
                        case init :+ last => init.foldRight(height1Map(last.event))((event, resHeight) => bigoplus(height0Map(event.event), resHeight))
                    }
                    height1Map.put(x.event, x1Height)

                    val x1Probability: Seq[Probability] = nonEmptyInits(rank).flatMap {
                        case init :+ last => init.foldRight(probabilities1Map(last.event))((event, resProb) => bigotimes(probabilities0Map(event.event), resProb))
                    }
                    probabilities1Map.put(x.event, x1Probability)

                    val x0Height: Seq[Int] = rank.map(event => height0Map(event.event)).reduce(bigoplus)
                    height0Map.put(x.event, x0Height)

                    val x0Probability: Seq[Probability] = rank.map(event => probabilities0Map(event.event)).reduce(bigotimes)
                    probabilities0Map.put(x.event, x0Probability)
            }
        }
    }

    vectorMultiply(
        height0Map(tree.event).map(_.toDouble),
        probabilities0Map(tree.event)
    ) +
        vectorMultiply(
            height1Map(tree.event).map(_.toDouble),
            probabilities1Map(tree.event))
}

@main def main7(): Unit = {
    // 2.35
    val tree = FaultTree.AndEvent('g', Seq(
        FaultTree.OrEvent('e', Seq(
            FaultTree.BasicEvent('a', 1D/2D),
            FaultTree.BasicEvent('b', 1D/3D)
        )),
        FaultTree.OrEvent('f', Seq(
            FaultTree.BasicEvent('c', 1D/4D),
            FaultTree.BasicEvent('d', 1D/5D)
        ))
    ))

//     1.375
//        val tree = FaultTree.AndEvent('a', Seq(
//            FaultTree.OrEvent('o', Seq(
//                FaultTree.BasicEvent(0, 1D/2D),
//                FaultTree.BasicEvent(1, 1D/3D)
//            )),
//            FaultTree.BasicEvent(2, 1D/4D)
//        ))

    println(height(tree))
}
