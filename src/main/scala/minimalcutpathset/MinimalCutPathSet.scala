package minimalcutpathset

import java.util.random.RandomGenerator

type Event = Int
type Probability = Double

enum Decision:
    case Left   // 0
    case Right  // 1
type Path = List[Decision]

type CutSets = Seq[Set[Event]]
type PathSets = Seq[Set[Event]]

// TODO could probably use Trie data-structure for better efficiency.

type Eta = Event | Decision
type Etas = scala.collection.mutable.Map[Path, Eta]

type Vertices = scala.collection.mutable.Map[Path, Set[Event]]

type Height = 0 | 1 | Function0[Double]
type Heights = scala.collection.mutable.Map[Path, Height]

def asDouble(height: Height): Double = height match
    case 0: 0 => 0D
    case 1: 1 => 1D
    case f: Function0[Double] => f()

def nonZero(height: Height): Boolean = height match
    case 0: 0 => false
    case 1: 1 => true
    case f: Function0[Double] => true

def other(decision: Decision): Decision = decision match
    case Decision.Left => Decision.Right
    case Decision.Right => Decision.Left

def approximate1(minimalCutsets: CutSets, minimalPathsets: PathSets, basicEvents: Seq[Probability], random: RandomGenerator): (Etas, Double) = {
    val n = basicEvents.size

    val etas: Etas = scala.collection.mutable.Map.empty
    val vertices: Vertices = scala.collection.mutable.Map.empty[Path, Set[Event]].withDefaultValue(Set())
    val heights: Heights = scala.collection.mutable.Map.empty
    val X = scala.collection.mutable.Map.empty[Int, Set[Path]].withDefaultValue(Set())  // paths, by layer
    X.update(0, Set(List()))

    // TODO re-enable this
    //val a: Event = random.nextInt(basicEvents.size)
    val a: Event = 1 // B
    etas.update(List(), a)

    val probA: Probability = basicEvents(a)
    val hNil: Function0[Double] = new CachedFunction0(() => 1D + (1D - probA) * asDouble(heights(List(Decision.Left))) + probA * asDouble(heights(List(Decision.Right))))
    heights.update(List(), hNil)

    for i <- 0 to (n - 2) do
        if X(i).nonEmpty then
            println(s"DEBUG X_${i} = ${X.getOrElse(i, Set())}")

            for x <- X(i) do
                for j <- Seq[Decision](Decision.Right, Decision.Left) do
                    val s: Path = j :: x        // x = 0, j = 1, s = 10
                    val t: Path = x.dropWhile(_ == other(j))    // t = []

                    val etaX: Event = etas(x).asInstanceOf[Event]
                    val Vs: Set[Event] = vertices(t) + etaX
                    vertices.put(s, Vs)

                    if (s == List(Decision.Left, Decision.Left)) {
                        println(s"V_00 = ${Vs}")
                    }
                    if (s == List(Decision.Left)) {
                        println(s"V_0 = ${Vs}")
                    }
                    if (s == List(Decision.Right, Decision.Left)) {
                        println(s"V_10 = ${Vs}")
                    }

                    // TODO can we compute this more efficiently?
                    val Bs: Seq[Event] = if j == Decision.Right then
                        // { x | x <- cutset, cutset <- cutsets, Vs subSet cutset, x notIn Vs}
                        for { cutSet <- minimalCutsets; if Vs.subsetOf(cutSet); x <- cutSet; if !Vs.contains(x) } yield x
                    else
                        // { x | x <- cutset, cutset <- cutsets, Vs subSet cutset, x notIn Vs}
                        for { pathSet <- minimalPathsets; if Vs.subsetOf(pathSet); x <- pathSet; if !Vs.contains(x) } yield x

                    if (s == List(Decision.Left)) {
                        println(s"B_0 = ${Bs}")
                    }
                    if (s == List(Decision.Right)) {
                        println(s"B_1 = ${Bs}")
                    }
                    if (s == List(Decision.Right, Decision.Right)) {
                        println(s"B_11 = ${Bs}")
                    }
                    if (s == List(Decision.Left, Decision.Left)) {
                        println(s"B_00 = ${Bs}")
                    }

                    val (etaS: Eta, heightS: Height) = if Bs.nonEmpty then
                        val b = if j == Decision.Right then
                            Bs.maxBy(b => basicEvents(b))
                        else
                            Bs.minBy(b => basicEvents(b))
                        val probB = basicEvents(b)
                        X.updateWith(i + 1) {
                            case Some(paths) => Some(paths + s);
                            case None => Some(Set(s))
                        }
                        (b: Eta, CachedFunction0(() => 1 +
                            (1 - probB) * asDouble(heights(Decision.Left :: s)) +
                            probB * asDouble(heights(Decision.Right :: s))))
                    else
                        println("DEBUG!!")
                        (j: Eta, 0: Height)

                    etas.update(s, etaS)
                    heights.update(s, heightS)

                    if (s == List(Decision.Right, Decision.Right)) {
                        println(s"eta_11 = ${etaS}");
                    }
                    if (s == List(Decision.Left)) {
                        println(s"eta_0 = ${etaS}")
                    }

                end for
            end for
        end if
    end for
    (etas, hNil())
}

// TODO JDK 25: use StableValue api.
class CachedFunction0[A](private var supplier: Function0[A]) extends Function0[A] {
    private var value: A | Unset.type = Unset

    override def apply(): A = {
        if value == Unset then
            value = supplier.apply()
            supplier = null

        value.asInstanceOf[A]
    }

    override def toString(): String = if value == Unset then String.valueOf(supplier) else String.valueOf(value)
}

object Unset

@main
def main(): Unit = {

    val cutsets = Seq(Set(0), Set(1, 2), Set(1, 3))
    val pathsets = Seq(Set(0, 1), Set(0, 2, 3))
    val probabilities = IndexedSeq(2D/3D, 1D/4D, 1D/3D, 1D/2D)

    val (etas, hNil) = approximate1(cutsets, pathsets, probabilities, new java.util.Random())

    println(etas)
    println(hNil)
}