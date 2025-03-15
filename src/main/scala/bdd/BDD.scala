package bdd

import guru.nidi.graphviz.model.{Link, MutableGraph, MutableNode}
import guru.nidi.graphviz.parse.Parser

import java.io.File
import scala.collection.immutable.IntMap
import scala.collection.mutable

/*
Explanation from Matthias regarding edges in the BDD:

The BDDs are represented internally in Sylvan as BDDs with complement
edges. Each edges which has a black dot at the source of the edge
indicates such a complement edge. Complement edges negate the
corresponding value. So if you have a complement edge leading to false
it actually represents true. Sylvan uses complement edges for a more
compact representation.
The BDDs should therefore be correct, they are just visualized differently.

In the dot file we have the following edge styles:
[style=solid dir=both arrowtail=dot]; <-- negated edge
[style=dashed];                       <-- regular edge

Matthias' explanation regarding the node order of the BDD:

The Sylvan export does not allow us to easily add more information to
the BDD nodes. So the Dot output just contains a simple enumeration of
the BDD nodes.
The BDD nodes are however always ordered according to their order in the
provided Galileo file. So the first BE in the Galileo file corresponds
to BDD node 0, etc.
Using your DFT file, I get the attached BDD, and for instance BDD node 0
corresponds to Signal_and_communication, BDD 1 to
Frogs,_switches_and_track_appliances, etc.

 */

enum BDD:
    case True
    case False
    case Node(event: Int, /*event fails*/ trueBranch: BDD, /*no failure*/ falseBranch: BDD)

object BDD {

    def height(bdd: BDD, probabilities: IntMap[Double]): Double = bdd match {
        case BDD.True => 0
        case BDD.False => 0
        case BDD.Node(event, trueBranch, falseBranch) =>
            val pk = probabilities(event)
            1 + pk * height(trueBranch, probabilities) + (1 - pk) * height(falseBranch, probabilities)
    }

    def bddProbabilities(faultTree: faulttree.FaultTree): IntMap[Double] = {
        // Note: events are numbered by the order of basic events in the .dft file which we fed to storm-dft.
        // So we have to use the same ordering for our BDD probabilities.
        ???
    }

    def bddProbabilities(faultTree: minimalcutpathset.FaultTree): IntMap[Double] = {
        // idem.
        // TODO maybe put this calculation in Conversion.scala?
        ???
    }

    def main(args: Array[String]): Unit = {
        val bdd = readStormSylvanBDDDotFile(new File("generated/bdd/AircraftRunwayExcursionAccidents.dot"))
        println(bdd)
    }

    enum EdgeType:
        case NotFailing //'false' or 'no failure' branches
        case Failing    //'true' or 'failure' branches

    def getEdgeType(edge: Link): EdgeType = {
        val attributes = edge.attrs()
        (attributes.get("style"), attributes.get("arrowtail")) match {
            case ("solid", "dot" | "none") => EdgeType.Failing
            case ("dashed", null) => EdgeType.NotFailing
        }
    }

    def readStormSylvanBDDDotFile(file: File): BDD = {
        // https://github.com/nidi3/graphviz-java#user-content-parsing
        val graph: MutableGraph = new Parser().read(file)

        import scala.jdk.CollectionConverters.given

        val nodesByName = graph.nodes().asScala.map(node => node.name().toString -> node).toMap

        val topNode = graph.nodes().stream().filter(node => "0".equals(node.attrs().get("label").toString)).findFirst().get()

        getBDDNode(topNode, new mutable.HashMap[String, BDD](), nodesByName)
    }

    def getChildren(node: MutableNode, byName: Map[String, MutableNode]): (/*true*/MutableNode, /*false*/MutableNode) = {
        val links = node.links()

        val fst = links.get(0)
        val snd = links.get(1)

        val fstEdgeType = getEdgeType(fst)
        val sndEdgeType = getEdgeType(snd)

        val fstTarget = byName(fst.to().name().toString)
        val sndTarget = byName(snd.to().name().toString)

        assert(fstEdgeType != sndEdgeType)

        fstEdgeType match {
            case EdgeType.Failing => (fstTarget, sndTarget)
            case EdgeType.NotFailing => (sndTarget, fstTarget)
        }
    }

    def getBDDNode(node: MutableNode, cache: mutable.Map[String, BDD], byName: Map[String, MutableNode]): BDD = {
        val nodeName = node.name().toString
        cache.get(nodeName) match {
            case Some(bdd) => bdd
            case None =>
                val (childTrue, childFalse) = getChildren(node, byName)
                val bddTrue = if "F".equals(childTrue.get("label").toString) then BDD.True else getBDDNode(childTrue, cache, byName)
                val bddFalse = if "F".equals(childFalse.get("label").toString) then BDD.False else getBDDNode(childFalse, cache, byName)
                val bdd = BDD.Node(getBDDId(node), bddTrue, bddFalse)
                cache.put(nodeName, bdd)
                bdd
        }
    }

    def getBDDId(node: MutableNode): Int = Integer.parseInt(node.get("label").toString)

}
