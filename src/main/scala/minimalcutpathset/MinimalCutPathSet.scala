package minimalcutpathset

import java.util.random.RandomGenerator

type Event = Int
type Probability = Double

type Decision = 0/*left*/ | 1/*right*/
type Path = List[Decision]

type CutSets = Seq[Set[Event]]
type PathSets = Seq[Set[Event]]

type Eta = Event | Boolean
type Etas = scala.collection.mutable.Map[Path, Eta]

type Vertices = scala.collection.mutable.Map[Path, Set[Event]]

type Height = 0 | 1 | Double | Function0[Double]
type Heights = scala.collection.mutable.Map[Path, Height]

def asDouble(height: Height): Double = height match
    case 0: 0 => 0D
    case 1: 1 => 1D
    case h: Double => h
    case f: Function0[Double] => f()

def nonZero(height: Height): Boolean = height match
    case 0: 0 => false
    case 1: 1 => true
    case h: Double => h != 0D
    case f: Function0[Double] => false

def other(decision: Decision): Decision = (1 - decision).asInstanceOf[Decision]

def approximate1(minimalCutsets: CutSets, minimalPathsets: PathSets, basicEvents: Seq[Probability], random: RandomGenerator): (Etas, Double) = {
    val etas: Etas = scala.collection.mutable.Map.empty
    val vertices: Vertices = scala.collection.mutable.Map.empty
    val heights: Heights = scala.collection.mutable.Map.empty

    val a: Event = random.nextInt(basicEvents.size)
    val probA: Probability = basicEvents(a)
    def hNil: Double = 1D + (1D - probA) * asDouble(heights(List(0))) + probA * asDouble(heights(List(1)))

    var s: Path = Nil

    while nonZero(heights(s)) do
        val x: Path = s;
        for j <- Seq[Decision](1, 0) do
            s = j :: x
            val t: Path = x.dropWhile(_ == other(j))

            val etaX: Event = etas(x).asInstanceOf[Event]
            val Vs: Set[Event] = vertices(t) + etaX
            vertices.put(s, Vs)

            // TODO can we compute this more efficiently?
            val Bs: Seq[Event] = if j == 1 then
                // { x | x <- cutset, cutset <- cutsets, Vs subSet cutset, x notIn Vs}
                for {cutSet <- minimalCutsets; if Vs.subsetOf(cutSet); x <- cutSet; if !Vs.contains(x) } yield x
            else
                // { x | x <- cutset, cutset <- cutsets, Vs subSet cutset, x notIn Vs}
                for { pathSet <- minimalPathsets; if Vs.subsetOf(pathSet); x <- pathSet; if !Vs.contains(x) } yield x

            val (etaS: Event, heightS: Height) = if Bs.nonEmpty then
                val (probB, b) = if j == 1 then
                    basicEvents.zipWithIndex.maxBy { case (p: Probability, basicEvent: Event) => p }
                else
                    basicEvents.zipWithIndex.minBy { case (p: Probability, basicEvents: Event) => p }
                (b, CachedFunction0(() => 1 + (1 - probB) * asDouble(heights(0 :: s)) + probB * asDouble(heights(1 :: s))))
            else
                (List(j), 0: 0)

            etas.update(s, etaS)
            heights.update(s, heightS)
        end for
    end while

    (etas, hNil)
}

class CachedFunction0[A](private var supplier: Function0[A]) extends Function0[A] {
    private var value: A | Unset.type = Unset

    override def apply(): A = {
        if value == Unset then
            value = supplier.apply()
            supplier = null

        value.asInstanceOf[A]
    }
}

object Unset
