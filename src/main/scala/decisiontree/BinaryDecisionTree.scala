package decisiontree

import scala.annotation.tailrec

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

enum Path:
    case Leaf(id: Event)
    case ConsLeft(id: Event, tail: Path)
    case ConsRight(id: Event, tail: Path)

    override def toString: String = this match
        case Leaf(id) => s"b_${id}"
        case ConsLeft(id, tail) => s"b_${id}l${tail}"
        case ConsRight(id, tail) => s"b_${id}r${tail}"

    def head: Event = this match
        case Leaf(id) => id
        case ConsLeft(id, _) => id
        case ConsRight(id, _) => id

    // TODO can cache results if computation becomes slow. Can use something like CachedFunction0, or lazy val.
    def last: Event = this match
        case Leaf(id) => id
        case ConsLeft(_, tail) => tail.last
        case ConsRight(_, tail) => tail.last

    def upTo(event: Event): Path = this match
        case p@Leaf(id) => if event == id then p else throw new NoSuchElementException("Not found: " + event)
        case ConsLeft(id, _) if event == id => Leaf(id)
        case ConsLeft(id, tail) => ConsLeft(id, tail.upTo(event))
        case ConsRight(id, _) if event == id => Leaf(id)
        case ConsRight(id, tail) => ConsRight(id, tail.upTo(event))

    def from(event: Event): Path = this match
        case p@Leaf(id) => if event == id then p else throw new NoSuchElementException("Not found: " + event)
        case p@ConsLeft(id, tail) => if event == id then p else tail.from(event)
        case p@ConsRight(id, tail) => if event == id then p else tail.from(event)

    def addLastLeft(event: Event): Path = this match
        case Leaf(id) => ConsLeft(id, Leaf(event))
        case ConsLeft(id, tail) => ConsLeft(id, tail.addLastLeft(event))
        case ConsRight(id, tail) => ConsRight(id, tail.addLastLeft(event))

    def addLastRight(event: Event): Path = this match
        case Leaf(id) => ConsRight(id, Leaf(event))
        case ConsLeft(id, tail) => ConsLeft(id, tail.addLastRight(event))
        case ConsRight(id, tail) => ConsRight(id, tail.addLastRight(event))

def getPaths(bdt: BinaryDecisionTree): LazyList[Path] = bdt match
    case BinaryDecisionTree.Zero => LazyList.empty
    case BinaryDecisionTree.One => LazyList.empty
    case BinaryDecisionTree.NonLeaf(id, l, r) =>
        if isLeafNode(l) && isLeafNode(r) then
            LazyList(Path.Leaf(id))
        else
            getPaths(l).map(pl => Path.ConsLeft(id, pl)) ++ getPaths(r).map(pr => Path.ConsRight(id, pr))

@tailrec
def isOnlyLeft(path: Path): Boolean = path match
    case Path.Leaf(_) => true
    case Path.ConsLeft(_, tail) => isOnlyLeft(tail)
    case Path.ConsRight(_, _) => false

@tailrec
def isOnlyRight(path: Path): Boolean = path match
    case Path.Leaf(_) => true
    case Path.ConsLeft(_, _) => false
    case Path.ConsRight(_, tail) => isOnlyRight(tail)

@tailrec
def traverse(binaryDecisionTree: BinaryDecisionTree, path: Path): BinaryDecisionTree = {
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

def replaceViaPath(top: BinaryDecisionTree, path: Path, replacement: BinaryDecisionTree): BinaryDecisionTree = top match {
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => top
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        assert:
            id == path.head

        path match
            case Path.Leaf(_) => replacement
            case Path.ConsLeft(_, tail) => BinaryDecisionTree.NonLeaf(id, replaceViaPath(left, tail, replacement), right)
            case Path.ConsRight(_, tail) => BinaryDecisionTree.NonLeaf(id, left, replaceViaPath(right, tail, replacement))
}

def replaceAfterPathLeft(top: BinaryDecisionTree, path: Path, replacement: BinaryDecisionTree): BinaryDecisionTree = top match {
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => replacement
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        assert:
            id == path.head

        path match
            case Path.Leaf(_) => BinaryDecisionTree.NonLeaf(id, replacement, right)
            case Path.ConsLeft(_, tail) => BinaryDecisionTree.NonLeaf(id, replaceAfterPathLeft(left, tail, replacement), right)
            case Path.ConsRight(_, tail) => BinaryDecisionTree.NonLeaf(id, left, replaceAfterPathLeft(right, tail, replacement))
}

def replaceAfterPathRight(top: BinaryDecisionTree, path: Path, replacement: BinaryDecisionTree): BinaryDecisionTree = top match {
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => replacement
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        assert:
            id == path.head

        path match
            case Path.Leaf(_) => BinaryDecisionTree.NonLeaf(id, left, replacement)
            case Path.ConsLeft(_, tail) => BinaryDecisionTree.NonLeaf(id, replaceAfterPathRight(left, tail, replacement), right)
            case Path.ConsRight(_, tail) => BinaryDecisionTree.NonLeaf(id, left, replaceAfterPathRight(right, tail, replacement))
}

def getPathsUpToRepeatedEvent(bdt: BinaryDecisionTree): LazyList[Path] = {

    def getPathsUpToRepeatedEvent(bdt: BinaryDecisionTree, seen: Set[Event]): LazyList[Path] = bdt match {
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

def getRepeatingPath(bdt: BinaryDecisionTree): Option[Path] =
    getPathsUpToRepeatedEvent(bdt).headOption

def eliminateOne(bdt: BinaryDecisionTree, path: Path): BinaryDecisionTree = {
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

object ExampleBDT {

    def main(args: Array[String]): Unit = {
        val tree = b(2, b(1, b(2, _0, _1), _1), b(1, b(1, b(2, _0, _1), _1), _1))

        val reducedTree = algorithm6(tree)

        println(reducedTree)
    }

    inline def b(id: Event, left: BinaryDecisionTree, right: BinaryDecisionTree): BinaryDecisionTree =
        BinaryDecisionTree.NonLeaf(id, left, right)
    inline def _0: BinaryDecisionTree = BinaryDecisionTree.Zero
    inline def _1: BinaryDecisionTree = BinaryDecisionTree.One
}