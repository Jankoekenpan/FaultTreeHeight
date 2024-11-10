package decisiontree

import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.collection.mutable

type Event = Int

enum BinaryDecisionTree:
    case Zero
    case One
    case NonLeaf(id: Event, left: BinaryDecisionTree, right: BinaryDecisionTree)

    override def toString: String = this match
        case Zero => "0"
        case One => "1"
        case NonLeaf(id, left, right) => s"b_${id}(${left},${right})"

def isLeafNode(binaryDecisionTree: BinaryDecisionTree): Boolean = binaryDecisionTree match
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => true
    case _ => false

object Path {
    case class Leaf[L](leaf: L) extends Path[L]:
        override def last: L = leaf
        override def length: Int = 1
    case class ConsLeft[L](id: Event, tail: Path[L]) extends Path[L]:
        override lazy val last: L = tail.last
        override lazy val length: Int = 1 + tail.length
    case class ConsRight[L](id: Event, tail: Path[L]) extends Path[L]:
        override lazy val last: L = tail.last
        override lazy val length: Int = 1 + tail.length
}

trait Path[L] {
    import Path.*

    override def toString: String = this match
        case Leaf(id) => s"b_${id}"
        case ConsLeft(id, tail) => s"b_${id}l${tail}"
        case ConsRight(id, tail) => s"b_${id}r${tail}"

    def head: Event | L = this match
        case Leaf(id) => id
        case ConsLeft(id, _) => id
        case ConsRight(id, _) => id

    def last: L
    def length: Int

    def upTo(event: Event): Path[Event] = this match
        case p@Leaf(id) => if event == id then p.asInstanceOf[Path[Event]] else throw new NoSuchElementException("Not found: " + event)
        case ConsLeft(id, _) if event == id => Leaf(id)
        case ConsLeft(id, tail) => ConsLeft(id, tail.upTo(event))
        case ConsRight(id, _) if event == id => Leaf(id)
        case ConsRight(id, tail) => ConsRight(id, tail.upTo(event))

    def from(event: Event): Path[Event] = this match
        case p@Leaf(id) => if event == id then p.asInstanceOf[Path[Event]] else throw new NoSuchElementException("Not found: " + event)
        case p@ConsLeft(id, tail) => if event == id then p.asInstanceOf[Path[Event]] else tail.from(event)
        case p@ConsRight(id, tail) => if event == id then p.asInstanceOf[Path[Event]] else tail.from(event)
}

@tailrec
def traverse(binaryDecisionTree: BinaryDecisionTree, path: Path[Event]): BinaryDecisionTree = {
    assert:
        binaryDecisionTree match
            case BinaryDecisionTree.NonLeaf(treeNodeId, _, _) => treeNodeId == path.head
            case _ => false

    val BinaryDecisionTree.NonLeaf(_, left, right) = binaryDecisionTree: @unchecked

    path match
        case Path.Leaf(_) => binaryDecisionTree
        case Path.ConsLeft(_, tail) => traverse(left, tail)
        case Path.ConsRight(_, tail) => traverse(right, tail)
}

def replaceViaPath(top: BinaryDecisionTree, path: Path[Event], replacement: BinaryDecisionTree): BinaryDecisionTree = top match {
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => top
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        assert:
            id == path.head

        path match
            case Path.Leaf(_) => replacement
            case Path.ConsLeft(_, tail) => BinaryDecisionTree.NonLeaf(id, replaceViaPath(left, tail, replacement), right)
            case Path.ConsRight(_, tail) => BinaryDecisionTree.NonLeaf(id, left, replaceViaPath(right, tail, replacement))
}

def replaceAfterPathLeft(top: BinaryDecisionTree, path: Path[Event], replacement: BinaryDecisionTree): BinaryDecisionTree = top match {
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => replacement
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        assert:
            id == path.head

        path match
            case Path.Leaf(_) => BinaryDecisionTree.NonLeaf(id, replacement, right)
            case Path.ConsLeft(_, tail) => BinaryDecisionTree.NonLeaf(id, replaceAfterPathLeft(left, tail, replacement), right)
            case Path.ConsRight(_, tail) => BinaryDecisionTree.NonLeaf(id, left, replaceAfterPathLeft(right, tail, replacement))
}

def replaceAfterPathRight(top: BinaryDecisionTree, path: Path[Event], replacement: BinaryDecisionTree): BinaryDecisionTree = top match {
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => replacement
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        assert:
            id == path.head

        path match
            case Path.Leaf(_) => BinaryDecisionTree.NonLeaf(id, left, replacement)
            case Path.ConsLeft(_, tail) => BinaryDecisionTree.NonLeaf(id, replaceAfterPathRight(left, tail, replacement), right)
            case Path.ConsRight(_, tail) => BinaryDecisionTree.NonLeaf(id, left, replaceAfterPathRight(right, tail, replacement))
}

def getPathsUpToRepeatedEvent(bdt: BinaryDecisionTree): LazyList[Path[Event]] = {

    def getPathsUpToRepeatedEvent(bdt: BinaryDecisionTree, seen: Set[Event]): LazyList[Path[Event]] = bdt match {
        case BinaryDecisionTree.One | BinaryDecisionTree.Zero => LazyList.empty
        case BinaryDecisionTree.NonLeaf(id, left, right) =>
            if seen.contains(id) then
                // event was already seen in a parent node, cut off the search.
                LazyList(Path.Leaf(id))
            else if isLeafNode(left) && isLeafNode(right) then
                // we reached a dead end in our search - this path has no repeated events.
                LazyList.empty
            else
                // not at a leaf node yet, continue searching.
                val biggerSeen = seen + id
                val leftPaths = getPathsUpToRepeatedEvent(left, biggerSeen)
                val rightPaths = getPathsUpToRepeatedEvent(right, biggerSeen)
                leftPaths.map(lp => Path.ConsLeft(id, lp)) ++ rightPaths.map(rp => Path.ConsRight(id, rp))
    }

    getPathsUpToRepeatedEvent(bdt, Set())
}

def getRepeatingPath(bdt: BinaryDecisionTree): Option[Path[Event]] =
    getPathsUpToRepeatedEvent(bdt).headOption

def eliminateOne(bdt: BinaryDecisionTree, path: Path[Event]): BinaryDecisionTree = {
    // v (but it's also equal to u)
    val duplicateEvent = path.last

    // u...v
    val pathInBetween = path.from(duplicateEvent)

    // top...u
    val pathUpToU = path.upTo(duplicateEvent)

    pathInBetween match
        case Path.ConsLeft(u, _) =>
            // replace the left branch of u by the left branch of v.
            val BinaryDecisionTree.NonLeaf(v, left, _) = traverse(bdt, path): @unchecked
            replaceViaPath(bdt, path, left)
        case Path.ConsRight(u, _) =>
            // replace the right branch of u by the right branch of v.
            val BinaryDecisionTree.NonLeaf(v, _, right) = traverse(bdt, path): @unchecked
            replaceViaPath(bdt, path, right)
}

def eliminateRepeatedly(bdt: BinaryDecisionTree): BinaryDecisionTree = {
    var tree = bdt

    var repeatingPath = getRepeatingPath(tree)
    while repeatingPath.nonEmpty do
        tree = eliminateOne(tree, repeatingPath.get)
        repeatingPath = getRepeatingPath(tree)
    end while

    tree
}

def cleanupLeaves(bdt: BinaryDecisionTree): BinaryDecisionTree = {
    def tighten(bdt: BinaryDecisionTree): BinaryDecisionTree = bdt match
        case BinaryDecisionTree.NonLeaf(_, BinaryDecisionTree.Zero, BinaryDecisionTree.Zero) => BinaryDecisionTree.Zero
        case BinaryDecisionTree.NonLeaf(_, BinaryDecisionTree.One, BinaryDecisionTree.One) => BinaryDecisionTree.One
        case _ => bdt

    bdt match
        case BinaryDecisionTree.NonLeaf(x, left, right) => tighten(BinaryDecisionTree.NonLeaf(x, cleanupLeaves(left), cleanupLeaves(right)))
        case _ => bdt
}

def algorithm6(bdt: BinaryDecisionTree): BinaryDecisionTree =
    cleanupLeaves(eliminateRepeatedly(bdt))

type OccurrenceProbability = Double
type Height = Double

enum Last:
    case Zero, One
type Path7 = Path[Last]

def getPaths(bdt: BinaryDecisionTree): Seq[Path7] = bdt match
    case BinaryDecisionTree.Zero => LazyList(Path.Leaf(Last.Zero))
    case BinaryDecisionTree.One => LazyList(Path.Leaf(Last.One))
    case BinaryDecisionTree.NonLeaf(id, l, r) =>
        getPaths(l).map(pl => Path.ConsLeft(id, pl)) ++ getPaths(r).map(pr => Path.ConsRight(id, pr))

def isPath0(path: Path7): Boolean = path.last == Last.Zero

// h just counts basic events (so no leaf nodes).
def h(path: Path7): Int = path.length - 1
//
//enum Direction:
//    case L, R
//case class Edge(start: Event, direction: Direction, dest: Event | Last)
//type Edges = Seq[Edge]

//def edges(path: Path7): List[Edge] = path match
//    case Path.Leaf(_) => Nil
//    case Path.ConsLeft(x, xs) => Edge(x, Direction.L, xs.head) :: edges(xs)
//    case Path.ConsRight(x, xs) => Edge(x, Direction.R, xs.head) :: edges(xs)

type Probability = Double
type BasicEvents = IntMap[Probability]

def vectorMultiply(one: Seq[Double], two: Seq[Double]): Double =
    one.lazyZip(two).map(_ * _).sum

def P(path: Path7, basicEventProbabilities: BasicEvents): Probability = path match {
    case Path.Leaf(_) => 1D
    case Path.ConsRight(event, tail) => basicEventProbabilities(event) * P(tail, basicEventProbabilities)
    case Path.ConsLeft(event, tail) => (1D - basicEventProbabilities(event)) * P(tail, basicEventProbabilities)
}

def algorithm7(bdt: BinaryDecisionTree, basicEvents: BasicEvents): (OccurrenceProbability, Height) = {
    val paths = getPaths(bdt)

    val (paths0, paths1) = paths.partition(isPath0)
    val h0 = for path0 <- paths0 yield h(path0).toDouble
    val h1 = for path1 <- paths1 yield h(path1).toDouble

    val p0 = for path0 <- paths0 yield P(path0, basicEvents)
    val p1 = for path1 <- paths1 yield P(path1, basicEvents)

    val rho: OccurrenceProbability = p1.sum
    val height: Height = vectorMultiply(h0, p0) + vectorMultiply(h1, p1)
    (rho, height)
}

import minimalcutpathset.FaultTree
import minimalcutpathset.TreeNode
import minimalcutpathset.Gate

def replaceZeros(tree: BinaryDecisionTree, replacement: BinaryDecisionTree): BinaryDecisionTree = tree match {
    case BinaryDecisionTree.Zero => replacement
    case BinaryDecisionTree.One => BinaryDecisionTree.One
    case BinaryDecisionTree.NonLeaf(id, left, right) => BinaryDecisionTree.NonLeaf(id, replaceZeros(left, replacement), replaceZeros(right, replacement))
}

def replaceOnes(tree: BinaryDecisionTree, replacement: BinaryDecisionTree): BinaryDecisionTree = tree match {
    case BinaryDecisionTree.Zero => BinaryDecisionTree.Zero
    case BinaryDecisionTree.One => replacement
    case BinaryDecisionTree.NonLeaf(id, left, right) => BinaryDecisionTree.NonLeaf(id, replaceOnes(left, replacement), replaceOnes(right, replacement))
}

def algorithm2(Tis: Seq[BinaryDecisionTree]): (BinaryDecisionTree, BinaryDecisionTree) = {
    def tau0(Tis: Seq[BinaryDecisionTree]): BinaryDecisionTree = Tis match {
        case Seq(tk) => tk
        case ti +: tis => replaceZeros(ti, tau0(tis))
    }

    def tau1(Tis: Seq[BinaryDecisionTree]): BinaryDecisionTree = Tis match {
        case Seq(tk) => tk
        case ti +: tis => replaceOnes(ti, tau1(tis))
    }

    (tau0(Tis), tau1(Tis))
}

def layers(tree: FaultTree): IArray[Seq[TreeNode]] = {
    val layersReversed: mutable.Map[Int, mutable.Seq[TreeNode]] = new mutable.TreeMap()

    def layerOf(t: TreeNode): Int = {
        val layer = t match {
            case TreeNode.BasicEvent(_, _) => 0
            case TreeNode.Combination(_, _, children) => children.toSeq.map(event => layerOf(tree.node(event))).max + 1
        }

        layersReversed.updateWith(layer) {
            case None => Some(mutable.IndexedSeq(t))
            case Some(seq) => Some(seq.appended(t))
        }

        layer
    }

    layerOf(tree.topNode)

    IArray.from(layersReversed.values.map(_.toSeq))
}

def basicTree(basicEvent: Event): BinaryDecisionTree.NonLeaf =
    BinaryDecisionTree.NonLeaf(basicEvent, BinaryDecisionTree.Zero, BinaryDecisionTree.One)

def algorithm8(faultTree: FaultTree): (BinaryDecisionTree, Height) =
    import minimalcutpathset.getProbabilities
    algorithm8(faultTree, getProbabilities(faultTree)())

def algorithm8(faultTree: FaultTree, basicEvents: BasicEvents): (BinaryDecisionTree, Height) =
    algorithm8(faultTree, basicEvents, layers(faultTree))

def algorithm8(faultTree: FaultTree, basicEvents: BasicEvents, layers: IArray[Seq[TreeNode]]): (BinaryDecisionTree, Height) = {
    val l = layers.length - 1

    val trees: mutable.Map[Event, BinaryDecisionTree] = new mutable.HashMap()
    for be <- basicEvents.keys do
        trees.put(be, basicTree(be))
    end for

    for j <- 1 to l do
        for TreeNode.Combination(x, gate, children) <- layers(j) do
            gate match
                case Gate.Or =>
                    val sortedChildTrees = children.toSeq
                        .map(c => (c, trees(c)))
                        .sortBy { case (event, tree) =>
                            val (rho, _) = algorithm7(tree, basicEvents); rho
                        } (using summon[Ordering[OccurrenceProbability]].reverse)
                        .map { case (_, tree) => tree }
                    val (tau0, _) = algorithm2(sortedChildTrees)
                    val Tx = algorithm6(tau0)
                    trees.put(x, Tx)
                case Gate.And =>
                    val sortedChildTrees = children.toSeq
                        .map(c => (c, trees(c)))
                        .sortBy { case (event, tree) =>
                            val (rho, _) = algorithm7(tree, basicEvents); rho
                        }
                        .map { case (_, tree) => tree }
                    val (_, tau1) = algorithm2(sortedChildTrees)
                    val Tx = algorithm6(tau1)
                    trees.put(x, Tx)
            end match
        end for
    end for

    val x = faultTree.topEvent
    val Tx = trees(x)
    val (_, heightX) = algorithm7(Tx, basicEvents)

    (Tx, heightX)
}

object ExampleBDT {

    def main(args: Array[String]): Unit = {
        val tree1 = b(2, b(1, b(2, _0, _1), _1), b(1, b(1, b(2, _0, _1), _1), _1))
        val reducedTree = algorithm6(tree1)
        println(reducedTree)

        // Figure 5, Example 8.
        val tree2 = b(3, b(1, _0, b(2, _0, _1)), b(2, b(1, _0, _1), _1))
        val probabilities = IntMap(1 -> 1D/4D, 2 -> 1D/2D, 3 -> 1D/3D)
        val (rho, h) = algorithm7(tree2, probabilities)
        println(rho)
        println(h)

        val tree3 = b(1, b(2, _0, _1), _1)
        val tree4 = b(3, b(4, _0, _1), _1)
        val tree5 = b(2, _0, _1)
        val (tau0, tau1) = algorithm2(Seq(tree3, tree4, tree5))
        println(tau0)
        println(tau1)

        import minimalcutpathset.getProbabilities
        val testTree = FaultTree(1, Map(
            1 -> TreeNode.Combination(1, Gate.Or, Set(2, 3)),
            2 -> TreeNode.Combination(2, Gate.And, Set(4, 5, 6)),
            3 -> TreeNode.Combination(3, Gate.And, Set(7, 8)),
            4 -> TreeNode.BasicEvent(4, 0.5),
            5 -> TreeNode.BasicEvent(5, 0.7),
            6 -> TreeNode.BasicEvent(6, 0.4),
            7 -> TreeNode.BasicEvent(7, 0.6),
            8 -> TreeNode.BasicEvent(8, 0.2)
        ))
        val basicEvents = getProbabilities(testTree)()
        val (bdt, height) = algorithm8(testTree, basicEvents)
        println(height) // expected: 2.632
    }

    inline def b(id: Event, left: BinaryDecisionTree, right: BinaryDecisionTree): BinaryDecisionTree =
        BinaryDecisionTree.NonLeaf(id, left, right)
    inline def _0: BinaryDecisionTree = BinaryDecisionTree.Zero
    inline def _1: BinaryDecisionTree = BinaryDecisionTree.One
}