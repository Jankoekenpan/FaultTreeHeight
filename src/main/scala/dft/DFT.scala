package dft

import scala.collection.mutable
import scala.io.Source
import stringmatching.regex.Interpolators.r // https://bishabosha.github.io/articles/simple-parsing-with-strings.html

enum DFTNode:
    case BasicEvent(id: Int, probability: Double)
    case OrEvent(id: Int, children: Seq[Int])
    case AndEvent(id: Int, children: Seq[Int])
    case TopLevel(id: Int)

object DFT {

    def readDFTFile(source: Source): Seq[DFTNode] = {
        var curId = 0
        def nextId(): Int = {
            val id = curId
            curId += 1
            id
        }

        val idMapping = new mutable.HashMap[String, Int]()
        def getId(event: String): Int = {
            idMapping.getOrElseUpdate(event, nextId())
        }

        val result = new mutable.ListBuffer[DFTNode]()

        for (line <- source.getLines()) {
            line match {
                case r"toplevel \"${topLevelEvent}\";" =>
                    result.addOne(DFTNode.TopLevel(getId(topLevelEvent)))
                case r"\"${parentEvent}\" or ${r"${childEvents}...( );"}" =>
                    result.addOne(DFTNode.OrEvent(getId(parentEvent), childEvents.map { case r"\"${childEvent}\"" => getId(childEvent) }))
                case r"\"${parentEvent}\" and ${r"${childEvents}...( );"}" =>
                    result.addOne(DFTNode.AndEvent(getId(parentEvent), childEvents.map { case r"\"${childEvent}\"" => getId(childEvent) }))
                case r"\"${basicEvent}\" prob=${r"$prob%g"};" =>
                    result.addOne(DFTNode.BasicEvent(getId(basicEvent), prob))
            }
        }

        result.toList
    }

    def main(args: Array[String]): Unit = {
        // TODO skip the following ones:
        //  assigning the risk 2.
        //  TODO which other ones?
        val source = Source.fromResource("AssessingtheRisks1.dft")

        val dftNodes = DFT.readDFTFile(source)

        for (node <- dftNodes) {
            println(node)
        }
    }

}
