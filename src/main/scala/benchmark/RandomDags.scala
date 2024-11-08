package benchmark

import minimalcutpathset.{FaultTree, Gate, TreeNode}

import java.util.random.RandomGenerator
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break;

type Event = Int

def randomGate()(using random: RandomGenerator): Gate =
    if random.nextBoolean() then Gate.And else Gate.Or

object RandomDags {

    // TODO test using ScalaCheck that the number of basic events of the generated DAG-like fault tree always equals nBasicEvents!
    def makeRandomDag(nBasicEvents: Int)(using random: RandomGenerator): FaultTree = {
        var id = 0

        def nextId(): Event = {
            val oldId = id
            id += 1
            oldId
        }

        // we have to ensure this is only a single top event
        // and we have to ensure there are no cycles in the graph.
        // we also have to ensure every basic event is reached
        // and we have to randomise the number edges.
        // also make sure that basic event's cannot have children.

        // idea:
        // build graph layer by layer.
        // each vertex from the child layer picks *exactly one* parent.
        // then each parent connects randomly to any vertex which is below.
        //
        // if a node in the new layer has only a single child at this point, we either have to
        //  - remove it
        //  - or force another child
        // we can decide it randomly (50% chance)
        //
        // stop after a layer of size 1 is generated

        val basicEvents: Seq[Event] = for _ <- 0 until nBasicEvents yield nextId()

        val downwardRelations = new mutable.HashMap[Event, mutable.Set[Event]]()
        val upwardRelations = new mutable.HashMap[Event, mutable.Set[Event]]()

        def addLink(parent: Event, child: Event): Unit = {
            val children = downwardRelations.getOrElseUpdate(parent, new mutable.HashSet[Event]())
            children.add(child)

            val parents = upwardRelations.getOrElseUpdate(child, new mutable.HashSet[Event]())
            parents.add(parent)
        }

        def hasSingleChild(parent: Event): Boolean = downwardRelations.get(parent).size == 1

        def removeLinks(event: Event): Unit = {
            downwardRelations.remove(event) match
                case Some(children) =>
                    for child <- children do
                        upwardRelations.get(child) match
                            case Some(parents) => parents.remove(event)
                            case None =>
                    end for
                case None =>
        }

        // We can be sure all basic events are from 0 until nBasicEvents.

        var previousLayer: Seq[Event] = basicEvents
        val descendants = mutable.ArrayBuffer.from(basicEvents)

        val topEvent: Event = boundary {
            while true do
                val newLayerEvents = mutable.ArrayBuffer.from(for _ <- 0 until newLayerSize(previousLayer.size) yield nextId())

                // for all vertices in previous layer, connect to at least one from the new layer.
                for child <- previousLayer do
                    val parent = pickRandomly(newLayerEvents)
                    addLink(parent, child)
                end for

                // for all combinations of new layer nodes and existing nodes, connect them randomly.
                for parent <- newLayerEvents; child <- descendants do
                    if random.nextBoolean() then
                        addLink(parent, child)
                    end if
                end for

                // If a node in the new layer has only one child at this point, we either have to remove it or force a second child
                // TODO should probably remove this part. may need to do some tuning at the end (remove intermediate nodes with only 1 child)
//                val removeFromLayer = new mutable.HashSet[Event]()
//                for parent <- newLayerEvents do
//                    if hasSingleChild(parent) then
//                        if random.nextBoolean() then
//                            // remove it
//                            removeLinks(parent)
//                            removeFromLayer.add(parent)
//                        else
//                            // try to force a new child
//                            val from = mutable.ArrayBuffer.from(descendants)
//                            from.subtractOne(downwardRelations(parent).head)
//                            if from.nonEmpty then
//                                // add the new child
//                                val child = pickRandomly(from)
//                                addLink(parent, child)
//                            else
//                                // unfortunately, still have to remove it, because no potential other child exists!
//                                removeLinks(parent)
//                                removeFromLayer.add(parent)
//                            end if
//                        end if
//                    end if
//                end for
//                newLayerEvents.subtractAll(removeFromLayer)

                // If the new layer is only size 1, then we finish generating layers.
                if newLayerEvents.size == 1 then
                    break(newLayerEvents.head)

                // In rare occasions the new layer is empty, we have to account for this.
                // e.g. originally it was size 2 but both parents got removed because they had a single child.
                if newLayerEvents.nonEmpty then
                    previousLayer = newLayerEvents.toSeq
                    descendants.addAll(newLayerEvents)
            end while
            -1 // Unreachable
        }

        // TODO it seems that some

        // Create DAG-like fault tree based on links :)
        val nodes = makeTreeNodes(nBasicEvents, topEvent, downwardRelations)
        FaultTree(topEvent, nodes)
    }

    def newLayerSize(childLayerSize: Int)(using random: RandomGenerator): Int =
        1 + random.nextInt(childLayerSize)

    def pickRandomly[T](from: scala.collection.Seq[T])(using random: RandomGenerator): T =
        from.apply(random.nextInt(from.size))

    def makeTreeNodes(nBasicEvents: Int, topEvent: Event, downwardRelations: scala.collection.Map[Event, scala.collection.Set[Event]])(using random: RandomGenerator): Map[Event, TreeNode] = {
        val children = downwardRelations.getOrElse(topEvent, Set.empty)
        val res = mutable.HashMap(topEvent -> makeTreeNode(nBasicEvents, topEvent, children))

        for c <- children do
            res.addAll(makeTreeNodes(nBasicEvents, c, downwardRelations))
        end for

        res.toMap
    }

    def makeTreeNode(nBasicEvents: Int, event: Event, children: scala.collection.Set[Event])(using random: RandomGenerator): TreeNode = {
        def isBasicEvent(event: Event): Boolean =
            0 <= event && event < nBasicEvents

        if isBasicEvent(event) then
            TreeNode.BasicEvent(event, randomProbability())
        else
            TreeNode.Combination(event, randomGate(), children.toSet)
    }

    def main(args: Array[String]): Unit = {
        given random: RandomGenerator = new java.util.Random()

        for (_ <- 0 until 10) {
            println(makeRandomDag(10))
        }
    }
}
