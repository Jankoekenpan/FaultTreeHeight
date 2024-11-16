package viz

import benchmark.RandomDags
import guru.nidi.graphviz.attribute.Rank
import guru.nidi.graphviz.engine.{Format, Graphviz}
import minimalcutpathset.FaultTree
import minimalcutpathset.TreeNode
import guru.nidi.graphviz.model.{MutableGraph, MutableNode}
import guru.nidi.graphviz.model.Factory.*

import java.io.File
import java.util.random.RandomGenerator
import javax.swing.{ImageIcon, JFrame, JLabel}
import scala.collection.mutable

type Event = Int

object FaultDAGViz {

    def toGraphViz(faultDag: FaultTree): MutableGraph = {
        val graph = mutGraph()
        graph.setDirected(true)
        graph.graphAttrs().add(Rank.dir(Rank.RankDir.TOP_TO_BOTTOM))

        val vertices = new mutable.HashMap[Event, MutableNode]()

        for (event, treeNode) <- faultDag.events do
            val label = treeNode match
                case TreeNode.BasicEvent(id, probability) => id + ": " + f"$probability%.6f"
                case TreeNode.Combination(id, gate, children) => id + ": " + gate
            val vertex = mutNode(label)
            vertices.put(event, vertex)
            graph.add(vertex)
        end for

        for treeNode <- faultDag.events.values do
            treeNode match
                case TreeNode.Combination(id, gate, children) =>
                    val src = vertices(id)
                    for child <- children do
                        val dst = vertices(child)
                        src.addLink(dst)
                    end for
                case TreeNode.BasicEvent(id, probabiltiy) =>
        end for

        graph
    }

    def display(it: Int, graph: MutableGraph): Unit = {
        val image = Graphviz.fromGraph(graph)
            .width(1920)
            .render(Format.PNG)
            //.toImage
            .toFile(new File(s"tmp/FaultDAG${it}.png"))

//        val icon = new ImageIcon(image)
//        val frame = new JFrame()
//        frame.setLayout(new FlowLayout())
//        frame.setSize(600, 600)
//        val lbl = new JLabel()
//        lbl.setIcon(icon)
//        frame.add(lbl)
//        frame.setVisible(true)
    }

    def display(fileName: String, faultTree: FaultTree): Unit = {
        val image = Graphviz.fromGraph(toGraphViz(faultTree))
            .width(1920)
            .render(Format.PNG)
            .toFile(new File(s"tmp/${fileName}.png"))
    }

    def main(args: Array[String]): Unit = {
//        given random: RandomGenerator = new java.util.Random()
//
//        for (i <- 1 to 10) {
//            display(i, toGraphViz(RandomDags.makeRandomDag(10)))
//        }

        display("T0 Chopper", reallife.T0Chopper.FT)
    }

}
