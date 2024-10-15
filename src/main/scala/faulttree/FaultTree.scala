package faulttree

import scala.collection.mutable

type Probability = Double
type Real = Double

type Event = Int

enum FaultTree(val event: Event):
    case BasicEvent(id: Event, probability: Probability) extends FaultTree(id)
    case AndEvent(id: Event, children: Seq[FaultTree]) extends FaultTree(id)
    case OrEvent(id: Event, children: Seq[FaultTree]) extends FaultTree(id)

def bigoplus[A](vectorLhs: Seq[A], vectorRhs: Seq[A])(using n: Numeric[A]): Seq[A] =
    for
        yk <- vectorRhs
        xk <- vectorLhs
    yield n.plus(xk, yk)

def bigotimes[A](vectorLhs: Seq[A], vectorRhs: Seq[A])(using n: Numeric[A]): Seq[A] =
    for
        yk <- vectorRhs
        xk <- vectorLhs
    yield n.times(xk, yk)

def vectorMultiply[A](vectorLhs: Seq[A], vectorRhs: Seq[A])(using n: Numeric[A]): A =
    vectorLhs.lazyZip(vectorRhs).map(n.times).reduce(n.plus)

def nonEmptyInits[A](seq: Seq[A]): Seq[Seq[A]] = inits(seq).tail

def inits[A](seq: Seq[A]): Seq[Seq[A]] = seq match
    case Nil => Seq(Seq())
    case x +: xs => Nil +: inits(xs).map(x +: _)

def layers(tree: FaultTree): IArray[Seq[FaultTree]] = {
    val layersReversed: mutable.Map[Int, mutable.Seq[FaultTree]] = new mutable.TreeMap()

    def layerOf(t: FaultTree): Int = {
        val layer = t match {
            case FaultTree.BasicEvent(_, _) => 0
            case FaultTree.AndEvent(_, children) => children.map(layerOf).max + 1
            case FaultTree.OrEvent(_, children) => children.map(layerOf).max + 1
        }

        layersReversed.updateWith(layer) {
            case None => Some(mutable.IndexedSeq(t))
            case Some(seq) => Some(seq.appended(t))
        }

        layer
    }

    layerOf(tree)

    IArray.from(layersReversed.values.map(_.toSeq))
}

def height(tree: FaultTree): Real = {
    height(tree, layers(tree))
}

def height(tree: FaultTree, layers: IArray[Seq[FaultTree]]): Real = {
    inline def eventsAtLayer(layer: Int): Seq[FaultTree] = layers(layer)
    inline def basicEvents: Seq[FaultTree.BasicEvent] = eventsAtLayer(0).asInstanceOf[Seq[FaultTree.BasicEvent]]

    val height0Map: mutable.Map[Event, Seq[Int]] = new mutable.HashMap()
    val height1Map: mutable.Map[Event, Seq[Int]] = new mutable.HashMap()

    val probabilities0Map: mutable.Map[Event, Seq[Probability]] = mutable.HashMap()
    val probabilities1Map: mutable.Map[Event, Seq[Probability]] = mutable.HashMap()

    def P(eventU: FaultTree): Probability = {
        val h1u = height1Map(eventU.event).sum
        val p1u = probabilities1Map(eventU.event).sum
        val h0u = height0Map(eventU.event).sum
        (h1u * p1u) / (h0u + h1u)
    }

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

@main def main(): Unit = {
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

    // 1.375
//    val tree = FaultTree.AndEvent('a', Seq(
//        FaultTree.OrEvent('o', Seq(
//            FaultTree.BasicEvent(0, 1D/2D),
//            FaultTree.BasicEvent(1, 1D/3D)
//        )),
//        FaultTree.BasicEvent(2, 1D/4D)
//    ))

    //println(height(tree))

    println(height(problematicTree))
}

val problematicTree: FaultTree = FaultTree.AndEvent(0, Seq(
    FaultTree.OrEvent(1, Seq(
        FaultTree.AndEvent(2, Seq(
            FaultTree.BasicEvent(3, 1D/3D),
            FaultTree.BasicEvent(4, 1D/4D)
        )),
        FaultTree.OrEvent(5, Seq(
            FaultTree.BasicEvent(6, 1D/6D),
            FaultTree.BasicEvent(7, 1D/7D)
        ))
    )),
    FaultTree.AndEvent(8, Seq(
        FaultTree.OrEvent(9, Seq(
            FaultTree.BasicEvent(10, 1D/10D),
            FaultTree.BasicEvent(11, 1D/11D)
        )),
        FaultTree.AndEvent(12, Seq(
            FaultTree.BasicEvent(13, 1D/13D),
            FaultTree.BasicEvent(14, 1D/14D)
        ))
    ))
))

val anotherTree = FaultTree.OrEvent(0, Seq(
    FaultTree.BasicEvent(1, 2D/3D),
    FaultTree.AndEvent(2, Seq(
        FaultTree.BasicEvent(3, 1D/4D),
        FaultTree.OrEvent(4, Seq(
            FaultTree.BasicEvent(5, 1D/3D),
            FaultTree.BasicEvent(6, 1D/2D)
        ))
    ))
))