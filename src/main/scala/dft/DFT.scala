package dft

import benchmark.Conversion

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source

enum DFTNode:
    case BasicEvent(id: Int, probability: Double)
    case OrEvent(id: Int, children: Seq[Int])
    case AndEvent(id: Int, children: Seq[Int])
    case TopLevel(id: Int)

object Printing {

    def print(dftNode: DFTNode): String = dftNode match {
        case DFTNode.TopLevel(id) => s"""toplevel "${id}";"""
        case DFTNode.OrEvent(id, children) => s""""${id}" or ${children.mkString("\"", "\" \"","\"")};"""
        case DFTNode.AndEvent(id, children) => s""""${id}" and ${children.mkString("\"", "\" \"","\"")};"""
        case DFTNode.BasicEvent(id, probability) => s""""${id}" prob=${probability};"""
    }

}

object Parsing {
    enum DFTLine:
        case BasicEvent(id: String, probability: String)
        case AndEvent(id: String, children: Seq[String])
        case OrEvent(id: String, children: Seq[String])
        case TopLevel(id: String)

    import fastparse.*
    import fastparse.NoWhitespace.*

    def dftToplevel[$: P]: P[DFTLine.TopLevel] =
        P("toplevel \"" ~ CharsWhile(_ != '\"').! ~ "\";")
            .map { x => DFTLine.TopLevel(x) }
    def dftOr[$: P]: P[DFTLine.OrEvent] =
        P("\"" ~ CharsWhile(_ != '\"').! ~ "\" or " ~ ("\"" ~ CharsWhile(_ != '\"').! ~ "\"").rep(sep=" ") ~ ";")
            .map { case (x, y) => DFTLine.OrEvent(x, y) }
    def dftAnd[$: P]: P[DFTLine.AndEvent] =
        P("\"" ~ CharsWhile(_ != '\"').! ~ "\" and " ~ ("\"" ~ CharsWhile(_ != '\"').! ~ "\"").rep(sep=" ") ~ ";")
            .map { case (x, y) => DFTLine.AndEvent(x, y) }
    def dftBasic[$: P]: P[DFTLine.BasicEvent] =
        P("\"" ~ CharsWhile(_ != '\"').! ~ "\" prob=" ~ CharsWhile(_ != ';').! ~ ";")
            .map { case (x, y) => DFTLine.BasicEvent(x, y) }
    def dftLine[$: P]: P[DFTLine] = dftToplevel | dftOr | dftAnd | dftBasic

    def parseDFTLine(line: String): DFTLine = {
        val Parsed.Success(parsed, _) = parse(line, dftLine)
        parsed
    }

    def main(args: Array[String]): Unit = {
        val topExample = "toplevel \"swag\";"
        val orExample = "\"asdf\" or \"gh  jkl;\" \"yolo\";"
        val andExample = "\"asdf\" and \"ghjkl;\" \"yolo\";"
        val basicExample = "\"swag\" prob=0.12345;"

        val Parsed.Success(foo, _) = parse(topExample, dftToplevel)
        val Parsed.Success(bar, _) = parse(orExample, dftOr)
        val Parsed.Success(qux, _) = parse(andExample, dftAnd)
        val Parsed.Success(baz, _) = parse(basicExample, dftBasic)

        val Parsed.Success(_, _) = parse(topExample, dftLine)
        val Parsed.Success(_, _) = parse(orExample, dftLine)
        val Parsed.Success(_, _) = parse(andExample, dftLine)
        val Parsed.Success(_, _) = parse(basicExample, dftLine)

        println(foo)
        println(bar)
        println(qux)
        println(baz)
    }

}

object DFT {

    def writeDFTFile(output: File, lines: Seq[DFTNode]): Unit = {
        val writer = new BufferedWriter(new FileWriter(output))
        for (line <- lines) {
            writer.write(Printing.print(line))
            writer.newLine()
        }
        writer.close()
    }

    def readDFTFile(source: Source): Seq[DFTNode] = {
        var curId = 0
        def nextId(): Int = {
            val id = curId
            curId += 1
            id
        }

        val idMapping = new mutable.HashMap[String, Int]()
        def getId(event: String): Int = {
            val id = idMapping.getOrElseUpdate(event, nextId())
            id
        }

        val result = new mutable.ListBuffer[DFTNode]()

        for (line <- source.getLines()) {
            Parsing.parseDFTLine(line) match {
                case Parsing.DFTLine.TopLevel(topLevelEvent) =>
                    result.addOne(DFTNode.TopLevel(getId(topLevelEvent)))
                case Parsing.DFTLine.OrEvent(parentEvent, childEvents) =>
                    result.addOne(DFTNode.OrEvent(getId(parentEvent), childEvents.map(getId)))
                case Parsing.DFTLine.AndEvent(parentEvent, childEvents) =>
                    result.addOne(DFTNode.AndEvent(getId(parentEvent), childEvents.map(getId)))
                case Parsing.DFTLine.BasicEvent(basicEvent, prob) =>
                    result.addOne(DFTNode.BasicEvent(getId(basicEvent), prob.toDouble))
            }
        }

        result.toList
    }

    def readTreeLikeFaultTree(source: Source): faulttree.FaultTree =
        Conversion.translateToTreeLikeFaultTree(readDFTFile(source))

    def readDagLikeFaultTree(source: Source): minimalcutpathset.FaultTree =
        Conversion.translateToDagTree(readDFTFile(source))
    
    def main(args: Array[String]): Unit = {
        val source = Source.fromResource("AssessingtheRisks1.dft")

        val dftNodes = DFT.readDFTFile(source)
        println(s"DEBUG: how many lines?: ${dftNodes.size}")

        for (node <- dftNodes) {
            println(node)
        }

        println(Conversion.translateToTreeLikeFaultTree(dftNodes))
    }

}
