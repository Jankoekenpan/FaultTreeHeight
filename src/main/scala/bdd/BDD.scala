package bdd

import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model.MutableGraph
import guru.nidi.graphviz.parse.Parser

import java.io.File
import scala.collection.immutable.IntMap

/*
Explanation from Matthias:

The BDDs are represented internally in Sylvan as BDDs with complement
edges. Each edges which has a black dot at the source of the edge
indicates such a complement edge. Complement edges negate the
corresponding value. So if you have a complement edge leading to false
it actually represents true. Sylvan uses complement edges for a more
compact representation.
The BDDs should therefore be correct, they are just visualized differently.

In the dot file we have the following edge styles:
[style=solid di r=both arrowtail=none]; <-- negated edge
[style=dashed];                         <-- regular edge
 */

enum BDD:
    case True
    case False
    case Node(id: Int, trueBranch: BDD, falseBranch: BDD)

object BDD {

    def height(bdd: BDD, probabilities: IntMap[Double]): Double = bdd match {
        case BDD.True => 0
        case BDD.False => 0
        case BDD.Node(id, trueBranch, falseBranch) =>
            val pk = probabilities(id)
            1 + pk * height(trueBranch, probabilities) + (1 - pk) * height(falseBranch, probabilities)
    }

    def readStormSylvanBDDDotFile(file: File): BDD = {
        // https://github.com/nidi3/graphviz-java#user-content-parsing
        val graph: MutableGraph = new Parser().read(file)

        graph.edges.forEach(edge => println(s"${edge.from}-->${edge.to()}"))

        ???
    }

    def main(args: Array[String]): Unit = {
        readStormSylvanBDDDotFile(new File("generated/bdd/AircraftRunwayExcursionAccidents.dot"))
    }
}

