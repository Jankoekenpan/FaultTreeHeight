package minimalcutpathset

import minimalcutpathset.TreeNode.Combination
import reallife.T0Chopper

import scala.collection.immutable.IntMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.given
import scala.util.boundary
import scala.util.boundary.break

enum Gate:
    case And
    case Or

enum TreeNode(id: Event):
    case Combination(id: Event, gate: Gate, children: Set[Event]) extends TreeNode(id)
    case BasicEvent(id: Event, probability: Probability) extends TreeNode(id)

case class FaultTree(topEvent: Event, events: Map[Event, TreeNode]):
    def node(id: Event): TreeNode = events(id)
    def topNode = node(topEvent)

type Event = Int
type Probability = Double

def flatten(faultTree: FaultTree): FaultTree = {
    val top = faultTree.topEvent

    val intermediateEventsWhichMightBeRemoved = mutable.Set.empty[Event]

    def flatten(event: Event, events: Map[Event, TreeNode]): Map[Event, TreeNode] = {
        val node = events(event)

        node match {
            case b: TreeNode.BasicEvent => events
            case TreeNode.Combination(orId, Gate.Or, orChildren) =>
                // look up the nodes of the orChildren,
                // if they themselves are also Or nodes, then:
                //     those grandchildren become the children of this Or node
                // and we also call flatten again on *all* children (need to thread events Map properly!!)

                var map = events
                var newOrChildren = orChildren

                for orChild <- orChildren do
                    map = flatten(orChild, map)
                    map(orChild) match
                        case TreeNode.Combination(_, Gate.Or, grandChildren) =>
                            newOrChildren = newOrChildren - orChild ++ grandChildren
                            intermediateEventsWhichMightBeRemoved.addOne(orChild)
                        case _ => // basic event or and node, no substitutions
                    end match
                end for
                val replacementNode = TreeNode.Combination(orId, Gate.Or, newOrChildren)
                map.updated(orId, replacementNode)
            case TreeNode.Combination(andId, Gate.And, andChildren) =>
                // idem

                var map = events
                var newAndChildren = andChildren

                for andChild <- andChildren do
                    map = flatten(andChild, map)
                    map(andChild) match
                        case TreeNode.Combination(_, Gate.And, grandChildren) =>
                            newAndChildren = newAndChildren - andChild ++ grandChildren
                            intermediateEventsWhichMightBeRemoved.addOne(andChild)
                        case _ => // basic event or or node, no substitutions
                    end match
                end for
                val replacementNode = TreeNode.Combination(andId, Gate.And, newAndChildren)
                map.updated(andId, replacementNode)
        }
    }

    var newEvents = flatten(top, faultTree.events)

    // Clean up orphan events
    def inverse(events: Map[Event, TreeNode]): Map[Event, Set[Event]] = {
        var res = Map.empty[Event, Set[Event]]

        for (_, node) <- events do
            node match
                case _: TreeNode.BasicEvent => // basic events are not parents, nothing to do
                case TreeNode.Combination(parent, _, children) =>
                    for child <- children do
                        res = res.updatedWith(child) {
                            case Some(parents) => Some(parents + parent)
                            case None => Some(Set(parent))
                        }
                    end for
        end for

        res
    }
    val parentRelation = inverse(newEvents)
    for toBeRemovedEvent <- intermediateEventsWhichMightBeRemoved do
        parentRelation.get(toBeRemovedEvent) match
            case Some(parents) if parents.nonEmpty =>
                // There is still a parent, we retain the toBeRemovedEvent in newEvents.
            case _ =>
                // No parents, remove orphan event from newEvents.
                newEvents = newEvents.removed(toBeRemovedEvent)
        end match
    end for

    FaultTree(top, newEvents)
}


enum Decision:
    case Zero   // 0
    case One    // 1
type Path = List[Decision]

type CutSet = Set[Event]
type CutSets = Set[CutSet]

type PathSet = Set[Event]
type PathSets = Set[PathSet]

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
    case Decision.Zero => Decision.One
    case Decision.One => Decision.Zero

def height(tree: FaultTree, probabilities: IntMap[Probability]): Double = {
    val basicEvents = getBasicEvents(tree)
    val cutSets = minimalCutSets(tree)(basicEvents)
    val pathSets = minimalPathSets(tree)(basicEvents)

    println(s"DEBUG cutSets = ${cutSets}")
    println(s"DEBUG pathSets = ${pathSets}")

    val (etas, height) = approximate(cutSets, pathSets, probabilities)

    height
}

def approximate(minimalCutSets: CutSets, minimalPathSets: PathSets, basicEvents: IntMap[Probability]): (Etas, Double) = {
    val n = basicEvents.size

    var minimumEtas: Etas = scala.collection.mutable.Map.empty
    var minimumHeight: Double = Double.PositiveInfinity

    for bk <- basicEvents.keys do
        val etas: Etas = scala.collection.mutable.Map.empty
        val vertices: Vertices = scala.collection.mutable.Map.empty[Path, Set[Event]].withDefaultValue(Set())
        val eventsOnPath: Vertices = scala.collection.mutable.Map.empty[Path, Set[Event]].withDefaultValue(Set())
        val heights: Heights = scala.collection.mutable.Map.empty
        val X = scala.collection.mutable.Map.empty[Int, Set[Path]].withDefaultValue(Set())  // paths, by layer
        X.update(0, Set(List()))

        etas.update(List(), bk)

        val probK: Probability = basicEvents(bk)
        val hNil: Function0[Double] = new CachedFunction0(() => 1D + (1D - probK) * asDouble(heights(List(Decision.Zero))) + probK * asDouble(heights(List(Decision.One))))
        heights.update(List(), hNil)

        for i <- 0 to (n - 1) do
            if X(i).nonEmpty then
                for x <- X(i) do
                    for j <- Seq[Decision](Decision.One, Decision.Zero) do
                        val s: Path = j :: x
                        val t: Path = x.dropWhile(_ == other(j))

                        val etaX: Event = etas(x).asInstanceOf[Event]
                        val Vs: Set[Event] = vertices(t) + etaX
                        vertices.put(s, Vs)

                        val Es: Set[Event] = eventsOnPath(x) + etaX
                        eventsOnPath.put(s, Es)

                        val (etaS: Eta, heightS: Height) =
                            if j == Decision.One then
                                if minimalCutSets.contains(Vs) || minimalCutSets.exists(cutSet => cutSet.subsetOf(Vs)) then
                                    val etaS: Eta = Decision.One
                                    val heightS: Height = 0
                                    (etaS, heightS)
                                else if minimalCutSets.exists(cutSet => Vs.subsetOf(cutSet)) then
                                    // TODO can we compute Bs more efficiently?
                                    var Bs = for { cutSet <- minimalCutSets; if Vs.subsetOf(cutSet); x <- cutSet; if !Es.contains(x) } yield x
                                    if Bs.isEmpty then
                                        Bs = basicEvents.keySet -- Es
                                    end if
                                    val b = Bs.maxBy(b => basicEvents(b))
                                    val probB = basicEvents(b)

                                    val etaS: Eta = b
                                    val heightS: Height = CachedFunction0(() => 1 +
                                        (1 - probB) * asDouble(heights(Decision.Zero :: s)) +
                                        probB * asDouble(heights(Decision.One :: s)))
                                    X.updateWith(i + 1) {
                                        case Some(paths) => Some(paths + s)
                                        case None => Some(Set(s))
                                    }
                                    (etaS, heightS)
                                else
                                    // TODO can we compute Bs more efficiently?
                                    var Bs = for { cutSet <- minimalCutSets; x <- cutSet; if !Es.contains(x); y <- (Vs - x); if cutSet.contains(y) } yield x
                                    if Bs.isEmpty then
                                       Bs = basicEvents.keySet -- Es
                                    end if

                                    val b = Bs.maxBy(b => basicEvents(b))
                                    val probB = basicEvents(b)

                                    val etaS: Eta = b
                                    val heightS: Height = CachedFunction0(() => 1 +
                                        (1 - probB) * asDouble(heights(Decision.Zero :: s)) +
                                        probB * asDouble(heights(Decision.One :: s)))
                                    X.updateWith(i + 1) {
                                        case Some(paths) => Some(paths + s)
                                        case None => Some(Set(s))
                                    }
                                    (etaS, heightS)
                                end if
                            else
                                if minimalPathSets.contains(Vs) || minimalPathSets.exists(pathSet => pathSet.subsetOf(Vs)) then
                                    val etaS: Eta = Decision.Zero
                                    val heightS: Height = 0
                                    (etaS, heightS)
                                else if minimalPathSets.exists(pathSet => Vs.subsetOf(pathSet)) then
                                    // TODO can we compute Bs more efficiently?
                                    var Bs = for { pathSet <- minimalPathSets; if Vs.subsetOf(pathSet); x <- pathSet; if !Es.contains(x) } yield x
                                    if Bs.isEmpty then
                                        Bs = basicEvents.keySet -- Es
                                    end if

                                    val b = Bs.minBy(b => basicEvents(b))
                                    val probB = basicEvents(b)

                                    val etaS: Eta = b
                                    val heightS: Height = CachedFunction0(() => 1 +
                                        (1 - probB) * asDouble(heights(Decision.Zero :: s)) +
                                        probB * asDouble(heights(Decision.One :: s)))
                                    X.updateWith(i + 1) {
                                        case Some(paths) => Some(paths + s)
                                        case None => Some(Set(s))
                                    }
                                    (etaS, heightS)
                                else
                                    // TODO can we compute Bs more efficiently?
                                    var Bs = for { pathSet <- minimalPathSets; x <- pathSet; if !Es.contains(x); y <- (Vs - x); if pathSet.contains(y) } yield x
                                    if Bs.isEmpty then
                                        Bs = basicEvents.keySet -- Es
                                    end if
                                    val b = Bs.minBy(b => basicEvents(b))
                                    val probB = basicEvents(b)

                                    val etaS: Eta = b
                                    val heightS: Height = CachedFunction0(() => 1 +
                                        (1 - probB) * asDouble(heights(Decision.Zero :: s)) +
                                        probB * asDouble(heights(Decision.One :: s)))
                                    X.updateWith(i + 1) {
                                        case Some(paths) => Some(paths + s)
                                        case None => Some(Set(s))
                                    }
                                    (etaS, heightS)
                                end if
                            end if
                        etas.update(s, etaS)
                        heights.update(s, heightS)
                    end for
                end for
            end if
        end for

        val heightNil = hNil()
        if heightNil < minimumHeight then
            minimumHeight = heightNil
            minimumEtas = etas
        end if
    end for

    (minimumEtas, minimumHeight)
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

    val tree = reproTree3
    val probabilities = IntMap.from(tree.events.collect { case (_, TreeNode.BasicEvent(e, p)) => (e, p) })

    println(height(tree, probabilities))

//    val cutSets = Set(Set(0), Set(1, 2), Set(1, 3))
//    val pathSets = Set(Set(0, 1), Set(0, 2, 3))
//    val probabilities = IntMap(0 -> 2D/3D, 1 -> 1D/4D, 2 -> 1D/3D, 3 -> 1D/2D)
//    val aBasicEvent = new java.util.Random().nextInt(probabilities.size)    // unexpected result for aBasicEvent = 0
//    println(aBasicEvent)
//
//    val (etas, hNil) = approximate(cutSets, pathSets, probabilities)
//
//    println(etas)
//    println(hNil)   // 1.4583333333333335 for aBasicEvent = 0
//
//    val exampleTree = FaultTree(0, Map(
//        0 -> TreeNode.Combination(0, Gate.Or, Set(1, 2)),
//        1 -> TreeNode.BasicEvent(1, 0D),    // A
//        2 -> TreeNode.Combination(2, Gate.And, Set(3, 4)),
//        3 -> TreeNode.BasicEvent(3, 0D),    // B
//        4 -> TreeNode.Combination(4, Gate.Or, Set(5, 6)),
//        5 -> TreeNode.BasicEvent(5, 0D),    // C
//        6 -> TreeNode.BasicEvent(6, 0D),    // D
//    ))

//    println(minimalCutSets(exampleTree))
//    println(minimalPathSets(exampleTree))
}

def minimalCutSets(faultTree: FaultTree)(basicEvents: Set[Event] = getBasicEvents(faultTree)): CutSets =
    MOCUS(faultTree)(basicEvents).toSet

def minimalPathSets(faultTree: FaultTree)(basicEvents: Set[Event] = getBasicEvents(faultTree)): PathSets =
    MOPAS(faultTree)(basicEvents).toSet

// TODO can we use this function in the MOCUS/MOPAS 'and' and 'or' branches instead of only at the end?
def removeSupersets(cutSets: Iterable[Set[Event]]): Seq[Set[Event]] = {
    val result = new java.util.HashSet[CutSet]()

    boundary {
        val cutSetIterator = cutSets.iterator
        while (cutSetIterator.hasNext) {
            val cutSet = cutSetIterator.next()
            val resIterator = result.iterator
            while (resIterator.hasNext) {
                val resSet = resIterator.next()
                if (resSet.subsetOf(cutSet)) {
                    break() // don't add cutSet to result
                } else if (cutSet.subsetOf(resSet)) {
                    resIterator.remove()
                }
            }
            result.add(cutSet)
        }
    }

    result.asScala.toList
}

def getProbabilities(faultTree: FaultTree)(basicEvents: Set[Event] = getBasicEvents(faultTree)): IntMap[Probability] =
    IntMap.from(basicEvents.map(e => faultTree.node(e)).map {
        case TreeNode.BasicEvent(be, prob) => (be, prob)
    })

def getBasicEvents(faultTree: FaultTree): Set[Event] =
    faultTree.events.filter {
            case (event: Event, basic: TreeNode.BasicEvent) => true
            case _ => false
        }
        .map((event, _) => event)
        .toSet

def MOCUS(faultTree: FaultTree)(basicEvents: Set[Event] = getBasicEvents(faultTree)): Seq[Set[Event]] = {

    // contains only sets which completely consist of basic evens
    val resultBuilder = new mutable.ListBuffer[Set[Event]]()

    // contains sets which contain non-basic events
    var ListOfCutSets: Seq[Set[Event]] = List(Set(faultTree.topEvent))

    while ListOfCutSets.nonEmpty do
        val C: Set[Event] = ListOfCutSets.head
        ListOfCutSets = ListOfCutSets.tail

        if C.subsetOf(basicEvents) then
            resultBuilder.addOne(C)
        else
            val eventIterator = C.iterator
            boundary:
                while eventIterator.hasNext do
                    val E = eventIterator.next()
                    faultTree.node(E) match
                        case TreeNode.Combination(_, Gate.And, children) =>
                            val Cnew: Set[Event] = C.excl(E).union(children)
                            ListOfCutSets = Cnew +: ListOfCutSets
                            break()
                        case TreeNode.Combination(_, Gate.Or, children) =>
                            val CwithoutE = C.excl(E)
                            val expandedSets = for c <- children yield CwithoutE.incl(c)
                            ListOfCutSets = ListOfCutSets.prependedAll(expandedSets)
                            break()
                        case _: TreeNode.BasicEvent =>
                            // nothing to do, continue with next element in this set.
                    end match
                end while
        end if
    end while

    removeSupersets(resultBuilder)
}

def MOPAS(faultTree: FaultTree)(basicEvents: Set[Event] = getBasicEvents(faultTree)): Seq[Set[Event]] = {

    // contains only sets which completely consist of basic evens
    val resultBuilder = new mutable.ListBuffer[Set[Event]]()

    // contains sets which contain non-basic events
    var ListOfPathSets: Seq[Set[Event]] = List(Set(faultTree.topEvent))

    while ListOfPathSets.nonEmpty do
        val P: Set[Event] = ListOfPathSets.head
        ListOfPathSets = ListOfPathSets.tail

        if P.subsetOf(basicEvents) then
            resultBuilder.addOne(P)
        else
            val eventIterator = P.iterator
            boundary:
                while eventIterator.hasNext do
                    val E = eventIterator.next()
                    faultTree.node(E) match
                        case TreeNode.Combination(_, Gate.Or, children) =>
                            val Pnew: Set[Event] = P.excl(E).union(children)
                            ListOfPathSets = Pnew +: ListOfPathSets
                            break()
                        case TreeNode.Combination(_, Gate.And, children) =>
                            val PwithoutE = P.excl(E)
                            val expandedSets = for c <- children yield PwithoutE.incl(c)
                            ListOfPathSets = ListOfPathSets.prependedAll(expandedSets)
                            break()
                        case _: TreeNode.BasicEvent =>
                            // nothing to do, continue with next element in this set.
                    end match
                end while
        end if
    end while

    removeSupersets(resultBuilder)
}

val reproTree = FaultTree(0, Map(
    0 -> TreeNode.Combination(0,Gate.Or,Set(1, 2, 6)),
    5 -> TreeNode.BasicEvent(5,0.22111687978494943),
    1 -> TreeNode.BasicEvent(1,0.43227359997383685),
    6 -> Combination(6,Gate.And,Set(7, 8, 9)),
    9 -> TreeNode.BasicEvent(9,0.9067115560033326),
    2 -> Combination(2,Gate.And,Set(3, 4, 5)),
    7 -> TreeNode.BasicEvent(7,0.7801324848806612),
    3 -> TreeNode.BasicEvent(3,0.852759862927743),
    8 -> TreeNode.BasicEvent(8,0.9046625728136396),
    4 -> TreeNode.BasicEvent(4,0.37254938544467475))
)

val inverseTree = FaultTree(0, Map(
    0 -> TreeNode.Combination(0,Gate.And,Set(1, 2, 6)),
    1 -> TreeNode.BasicEvent(1,0.43227359997383685),
    2 -> Combination(2,Gate.Or,Set(3, 4, 5)),
    3 -> TreeNode.BasicEvent(3,0.852759862927743),
    4 -> TreeNode.BasicEvent(4,0.37254938544467475),
    5 -> TreeNode.BasicEvent(5,0.22111687978494943),
    6 -> Combination(6,Gate.Or,Set(7, 8, 9)),
    7 -> TreeNode.BasicEvent(7,0.7801324848806612),
    8 -> TreeNode.BasicEvent(8,0.9046625728136396),
    9 -> TreeNode.BasicEvent(9, 0.9067115560033326),
))  // AND(1, OR(3, 4, 5), OR(7, 8, 9)) -- {{1, 3, 7}, {1, 3, 8}, {1, 3, 9}, {1, 4, 7}, {1, 4, 8}, {1, 4, 9}, {1, 5, 7}, {1, 5, 8}, {1, 5, 9}}.

val reproTree2 = FaultTree(0, Map(
    0 -> TreeNode.Combination(0,Gate.And,Set(1, 5, 8)),
    5 -> TreeNode.Combination(5,Gate.Or,Set(6, 7)),
    10 -> TreeNode.BasicEvent(10,0.29221086224595927),
    1 -> TreeNode.Combination(1,Gate.Or,Set(2, 3, 4)),
    6 -> TreeNode.BasicEvent(6,0.9054326731384866),
    9 -> TreeNode.BasicEvent(9,0.45262926590945163),
    2 -> TreeNode.BasicEvent(2,0.49269513586680935),
    7 -> TreeNode.BasicEvent(7,0.5186968563256549),
    3 -> TreeNode.BasicEvent(3,0.48008922637336704),
    11 -> TreeNode.BasicEvent(11,0.8682779082769414),
    8 -> TreeNode.Combination(8,Gate.Or,Set(9, 10, 11)),
    4 -> TreeNode.BasicEvent(4,0.4289924531062296)
))

val reproTree3 = FaultTree(0, Map(
    0 -> TreeNode.Combination(0,Gate.And,Set(1, 2, 6)),
    5 -> TreeNode.BasicEvent(5,0.0748941995542074),
    1 -> TreeNode.BasicEvent(1,0.2434053844309706),
    6 -> TreeNode.Combination(6,Gate.Or,Set(7, 8, 9)),
    9 -> TreeNode.BasicEvent(9,0.2846316951156139),
    2 -> TreeNode.Combination(2,Gate.Or,Set(3, 4, 5)),
    7 -> TreeNode.BasicEvent(7,0.43658420342890947),
    3 -> TreeNode.BasicEvent(3,0.9142663475183694),
    8 -> TreeNode.BasicEvent(8,0.6186034134816778),
    4 -> TreeNode.BasicEvent(4,0.20182555017352477)
))

@main def testSets(): Unit = {
    println(minimalCutSets(reproTree)())
    println(minimalPathSets(reproTree)())

    println(minimalCutSets(reproTree2)())
    println(minimalPathSets(reproTree2)())

    println(minimalCutSets(T0Chopper.FT))
}