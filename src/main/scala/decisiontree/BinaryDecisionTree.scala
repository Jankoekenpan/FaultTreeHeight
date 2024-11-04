package decisiontree

import decisiontree.Path.ConsLeft

import scala.annotation.tailrec

type Event = Int

enum BinaryDecisionTree:
    case Zero
    case One
    case NonLeaf(id: Event, left: BinaryDecisionTree, right: BinaryDecisionTree)

enum Path:
    case Leaf(id: Event)
    case ConsLeft(id: Event, tail: Path)
    case ConsRight(id: Event, tail: Path)

//enum Direction:
//    case Left
//    case Right

def getPaths(bdt: BinaryDecisionTree): Seq[Path] = bdt match
    case BinaryDecisionTree.Zero => Seq()
    case BinaryDecisionTree.One => Seq()
    case BinaryDecisionTree.NonLeaf(id, l, r) =>
        val pathsLeft = getPaths(l)
        val pathsRight = getPaths(r)
        pathsLeft.map(pl => Path.ConsLeft(id, pl)) ++ pathsRight.map(pr => Path.ConsRight(id, pr))

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

//def collectDirections(path: Path): Seq[Direction] = path match
//    case Path.Leaf(_) => Nil
//    case Path.ConsLeft(_, tail) => Direction.Left +: collectDirections(tail)
//    case Path.ConsRight(_, tail) => Direction.Right +: collectDirections(tail)
//
//def isOnlyLeft(directions: Seq[Direction]): Boolean = directions.forall(_ == Direction.Left)
//def isOnlyRight(directions: Seq[Direction]): Boolean = directions.forall(_ == Direction.Right)

def replaceSubtree(tree: BinaryDecisionTree, eventToReplace: Event, replacement: BinaryDecisionTree): BinaryDecisionTree = tree match {
    case BinaryDecisionTree.Zero => BinaryDecisionTree.Zero
    case BinaryDecisionTree.One => BinaryDecisionTree.One
    case BinaryDecisionTree.NonLeaf(id, left, right) =>
        if id == eventToReplace then
            replacement
        else
            BinaryDecisionTree.NonLeaf(id,
                replaceSubtree(left, eventToReplace, replacement),
                replaceSubtree(right, eventToReplace, replacement))
}

//TODO return the slice between the repeated vertices directly (as Path).
def repeatedVertex(path: Path): Option[Event] = {
    @tailrec
    def repeatedVertex(path: Path, seen: Set[Event]): Option[Event] = path match {
        case Path.Leaf(id) => if seen.contains(id) then Some(id) else None
        case Path.ConsLeft(id, tail) => repeatedVertex(tail, seen + id)
        case Path.ConsRight(id, tail) => repeatedVertex(tail, seen + id)
    }

    repeatedVertex(path, Set())
}

def eliminate(bdt: BinaryDecisionTree): BinaryDecisionTree = {
    for (path <- getPaths(bdt)) {
        repeatedVertex(path) match {
            case Some(repeatedVertexId) =>
                //TODO
                ???
            case None =>
                //TODO
                ???
        }
    }

    //TODO
    ???
}