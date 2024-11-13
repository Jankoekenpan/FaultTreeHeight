package benchmark

import minimalcutpathset.{FaultTree, Gate, TreeNode}

import java.nio.file.{Files, Path, StandardOpenOption}
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
//                if newLayerEvents.nonEmpty then TODO this can no longer occur
                assert(newLayerEvents.nonEmpty)
                previousLayer = newLayerEvents.toSeq
                descendants.addAll(newLayerEvents)
            end while
            -1 // Unreachable
        }

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

object DagPlots {

    def main(args: Array[String]): Unit = {
        given random: RandomGenerator = new java.util.Random()

        val csvOutput = DagCSVOutput.newDataFile()
        DagCSVOutput.printDataHeader(csvOutput)
        val csvTimeOutput = DagCSVOutput.newTimingsFile()
        DagCSVOutput.printTimingsHeader(csvTimeOutput)

        val nIterations = 50
        for (basicEvents <- 5 to 70/*TODO 85*/ by 5) {

//            var sumCutSet = 0.0
//            var sumPathSet = 0.0
//            var sumBDT = 0.0

//            var sumTimeCutSet_ns = 0L
//            var sumTimePathSet_ns = 0L
//            var sumTimeBDT_ns = 0L

            for (it <- 1 to nIterations) {
                println(s"Iteration: ${it}")

                val dagLikeFaultTree = RandomDags.makeRandomDag(basicEvents)
                println(s"DAG-like FT: ${dagLikeFaultTree}")
                val dagBasicEvents = minimalcutpathset.getBasicEvents(dagLikeFaultTree)
                println(s"basic events: ${dagBasicEvents}")
                val dagProbabilities = minimalcutpathset.getProbabilities(dagLikeFaultTree)(dagBasicEvents)

                val minimalCutSets = minimalcutpathset.minimalCutSets(dagLikeFaultTree)(dagBasicEvents)
                val minimalPathSets = minimalcutpathset.minimalPathSets(dagLikeFaultTree)(dagBasicEvents)
                println(s"minimalCutSets = ${minimalCutSets}")
                println(s"minimalPathSets = ${minimalPathSets}")

                println("Calculate height using CutSet algorithm...")
                val time1 = System.nanoTime()
                val heightCutSet = minimalcutpathset.algorithm4(minimalCutSets, dagProbabilities)._2
                val time1_end = System.nanoTime()
                println("Calculate height using PathSet algorithm...")
                val time2 = System.nanoTime()
                val heightPathSet = minimalcutpathset.algorithm5(minimalCutSets, dagProbabilities)._2
                val time2_end = System.nanoTime()
                println("Calculate height using Binary Decision Tree algorithm...")
                val time3 = System.nanoTime()
                val heightBDT = decisiontree.algorithm8(dagLikeFaultTree, dagProbabilities)._2
                val time3_end = System.nanoTime()
                println("Finished height calculations!")

                val cutset_ns = time1_end - time1
                val pathset_ns = time2_end - time2
                val bdt_ns = time3_end - time3

                val point = DagCoordinate(basicEvents, heightCutSet, heightPathSet, heightBDT)
                val time = DagTime(basicEvents, cutset_ns, pathset_ns, bdt_ns)

                DagCSVOutput.printData(csvOutput, point)
                DagCSVOutput.printTimings(csvTimeOutput, time)

//                sumCutSet += heightCutSet
//                sumPathSet += heightPathSet
//                sumBDT += heightBDT

//                sumTimeCutSet_ns += cutset_ns
//                sumTimePathSet_ns += pathset_ns
//                sumTimeBDT_ns += bdt_ns
            }

//            val averageHeightCutSet = sumCutSet / nIterations
//            val averageHeightPathSet = sumPathSet / nIterations
//            val averageHeightBDT = sumBDT / nIterations

            // TODO calculate average times too.

            // print to heights chart and timings chart...

            println(s"#basic events: ${basicEvents}")
            // TODO print some variables..
            println()
            println()
        }
    }
}

case class DagCoordinate(events: Int, heightCutSet: Double, heightPathSet: Double, heightBDT: Double)
case class DagTime(events: Int, timeCutSet_ns: Long, timePathSet_ns: Long, timeBDT_ns: Long)

object DagCSVOutput {

    private val outFile = "dag-points.csv"
    private val timingsFile = "dag-timings.csv"

    def newDataFile(): Path = {
        Files.createFile(Path.of(outFile))
    }

    def newTimingsFile(): Path = {
        Files.createFile(Path.of(timingsFile))
    }

    def printDataHeader(file: Path): Unit = {
        val line = "# Basic events,Height (cut set algorithm),Height (path set algorithm),Height (binary decision tree algorithm)\r\n"
        writeString(file, line)
    }

    def printTimingsHeader(file: Path): Unit = {
        val line = "# Basic events,Execution time (cut set algorithm) (ns),Execution time (path set algorithm) (ns),Execution time (binary decision tree algorithm) (ns)\r\n"
        writeString(file, line)
    }

    def printData(file: Path, point: DagCoordinate): Unit = point match
        case DagCoordinate(events, cutset, pathset, bdt) =>
            writeString(file, s""""$events","$cutset","$pathset","$bdt"\r\n""")

    def printTimings(file: Path, timings: DagTime): Unit = timings match
        case DagTime(events, cutset_ns, pathset_ns, bdt_ns) =>
            writeString(file, s""""$events","$cutset_ns","$pathset_ns","$bdt_ns"\r\n""")

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }
}