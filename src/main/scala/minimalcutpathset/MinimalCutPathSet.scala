package minimalcutpathset

import java.util.random.RandomGenerator

type Event = Int
type Probability = Double

type Decision = 0/*left*/ | 1/*right*/
type Path = List[Decision]

type CutSets = Seq[Set[Event]]
type PathSets = Seq[Set[Event]]

// TODO could probably use Trie data-structure for better efficiency.

type Eta = Event | Boolean
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

def other(decision: Decision): Decision = (1 - decision).asInstanceOf[Decision]

def approximate1(minimalCutsets: CutSets, minimalPathsets: PathSets, basicEvents: Seq[Probability], random: RandomGenerator): (Etas, Double) = {
    val etas: Etas = scala.collection.mutable.Map.empty
    val vertices: Vertices = scala.collection.mutable.Map.empty[Path, Set[Event]].withDefaultValue(Set())
    val heights: Heights = scala.collection.mutable.Map.empty

    // TODO re-enable this
    //val a: Event = random.nextInt(basicEvents.size)
    val a: Event = 1 // B
    etas.update(List(), a)

    val probA: Probability = basicEvents(a)
    val hNil: Function0[Double] = new CachedFunction0(() => 1D + (1D - probA) * asDouble(heights(List(0))) + probA * asDouble(heights(List(1))))
    heights.update(List(), hNil)

    var s: Path = Nil

    // TODO remove debug
    var count = 0

    while nonZero(heights(s)) && count < 10 do
        println("while")
        println(s"s = ${s}")
        val x: Path = s;
        println(s"x = ${x}")
        for j <- Seq[Decision](1, 0) do
            println("for")
            s = j :: x
            println(s"s = ${s}")
            val t: Path = x.dropWhile(_ == other(j))

            val etaX: Event = etas(x).asInstanceOf[Event]
            val Vs: Set[Event] = vertices(t) + etaX
            vertices.put(s, Vs)

            println(s"etaX = ${etaX}")  // TODO
            println(s"Vs = ${Vs}")      // TODO
            println(s"t = ${t}")        // TODO

            // TODO can we compute this more efficiently?
            val Bs: Seq[Event] = if j == 1 then
                // { x | x <- cutset, cutset <- cutsets, Vs subSet cutset, x notIn Vs}
                for {cutSet <- minimalCutsets; if Vs.subsetOf(cutSet); x <- cutSet; if !Vs.contains(x) } yield x
            else
                // { x | x <- cutset, cutset <- cutsets, Vs subSet cutset, x notIn Vs}
                for { pathSet <- minimalPathsets; if Vs.subsetOf(pathSet); x <- pathSet; if !Vs.contains(x) } yield x

            println(s"Bs = ${Bs}")

            val (etaS: Event, heightS: Height) = if Bs.nonEmpty then
                val (probB, b) = if j == 1 then
                    basicEvents.zipWithIndex.maxBy { case (p: Probability, basicEvent: Event) => p }
                else
                    basicEvents.zipWithIndex.minBy { case (p: Probability, basicEvents: Event) => p }
                (b, CachedFunction0(() => 1 + (1 - probB) * asDouble(heights(0 :: s)) + probB * asDouble(heights(1 :: s))))
            else
                println("DEBUG!!!") // TODO
                (List(j), 0: 0)

            etas.update(s, etaS)
            heights.update(s, heightS)

            println("end of for body")  // TODO
        end for
        //println(s"h = ${heights}")

        // TODO remove
        count += 1

        println("end of while body")
        println()
    end while

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