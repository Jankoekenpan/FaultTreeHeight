package decisiontree

import scala.annotation.tailrec

type Event = Int

enum BinaryDecisionTree:
    case Zero
    case One
    case NonLeaf(id: Event, left: BinaryDecisionTree, right: BinaryDecisionTree)

def isLeafNode(binaryDecisionTree: BinaryDecisionTree): Boolean = binaryDecisionTree match
    case BinaryDecisionTree.Zero | BinaryDecisionTree.One => true
    case _ => false

enum Path:
    case Leaf(id: Event)
    case ConsLeft(id: Event, tail: Path)
    case ConsRight(id: Event, tail: Path)

    def head: Event = this match
        case Leaf(id) => id
        case ConsLeft(id, _) => id
        case ConsRight(id, _) => id

    // TODO can cache results if computation becomes slow. Can use something like CachedFunction0.
    def last: Event = this match
        case Leaf(id) => id
        case ConsLeft(_, tail) => tail.last
        case ConsRight(_, tail) => tail.last

//    def init: Path = this match
//        case Leaf(_) => throw new NoSuchElementException("init of Leaf")
//        case ConsLeft(id, Leaf(_)) => Leaf(id)
//        case ConsLeft(id, tail) => ConsLeft(id, tail.init)
//        case ConsRight(id, Leaf(_)) => Leaf(id)
//        case ConsRight(id, tail) => ConsRight(id, tail.init)

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

//enum Direction:
//    case Left
//    case Right

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

def repeatedVertex(path: Path): Option[Event] = {
    @tailrec
    def repeatedVertex(path: Path, seen: Set[Event]): Option[Event] = path match {
        case Path.Leaf(id) => if seen.contains(id) then Some(id) else None
        case Path.ConsLeft(id, tail) => repeatedVertex(tail, seen + id)
        case Path.ConsRight(id, tail) => repeatedVertex(tail, seen + id)
    }

    repeatedVertex(path, Set())
}

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

// returned paths will always end with the duplicate event
def getRepeatingPath(bdt: BinaryDecisionTree): Option[Path] =
    getPaths(bdt)
        .map(path => (path, repeatedVertex(path)))
        .collectFirst { case (path, Some(duplicateEvent)) if path.last == duplicateEvent => path }

def eliminateOne(bdt: BinaryDecisionTree, path: Path): BinaryDecisionTree = {
    // v (but it's also equal to u)
    val duplicateEvent = path.last

    // u...v
    val pathInBetween = path.from(duplicateEvent)

    if isOnlyLeft(pathInBetween) then
        // replace v and its left and right branches by the left branch of v
        val BinaryDecisionTree.NonLeaf(v, left, _) = traverse(bdt, path): @unchecked
        replaceViaPath(bdt, path, left)
    else if isOnlyRight(pathInBetween) then
        // replace v and its left and right branches by the right branch of v
        val BinaryDecisionTree.NonLeaf(v, _, right) = traverse(bdt, path): @unchecked
        replaceViaPath(bdt, path, right)
    else
        val pathUpToU = path.upTo(duplicateEvent)

        pathInBetween match
            case Path.ConsRight(u, _) =>
                // replace the right branch of u by the right branch of v
                val BinaryDecisionTree.NonLeaf(v, _, right) = traverse(bdt, path): @unchecked
                replaceAfterPathRight(bdt, pathUpToU, right)
            case Path.ConsLeft(u, _) =>
                // replace the left branch of u by the left branch of v
                val BinaryDecisionTree.NonLeaf(v, left, _) = traverse(bdt, path): @unchecked
                replaceAfterPathLeft(bdt, pathUpToU, left)
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