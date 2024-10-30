package benchmark

import java.util.random.RandomGenerator
import faulttree.FaultTree
import faulttree.FaultTree.*

import scala.collection.mutable.ListBuffer

enum NodeType:
    case Basic, And, Or

object NodeType:
    val Values: Seq[NodeType] = Seq(NodeType.Basic, NodeType.And, NodeType.Or)

def randomNodeType()(using random: RandomGenerator): NodeType =
    random.nextInt(3) match
        case 0 => NodeType.Basic
        case 1 => NodeType.And
        case 2 => NodeType.Or

def randomProbability()(using random: RandomGenerator): Double =
    random.nextDouble(Double.MinPositiveValue, 1D)

object RandomTrees {

    def makeRandomTree(basicEvents: Int)(using random: RandomGenerator): FaultTree = {
        var id = 0

        def nextId(): Int = {
            val oldId = id
            id += 1
            oldId
        }

        val basicEventsNodes: Seq[FaultTree.BasicEvent] = for _ <- 0 until basicEvents yield FaultTree.BasicEvent(nextId(), randomProbability())

        val remainingEvents: ListBuffer[FaultTree] = ListBuffer.from(basicEventsNodes)

        def makeNode(nodeType: NodeType, children: IterableOnce[FaultTree]): FaultTree = nodeType match
            case NodeType.And => FaultTree.AndEvent(nextId(), Seq.from(children))
            case NodeType.Or => FaultTree.OrEvent(nextId(), Seq.from(children))

        while remainingEvents.sizeIs > 1 do
            val groupAmount = 2 + random.nextInt(remainingEvents.size - 1)
            val randomNodeType = if random.nextBoolean() then NodeType.And else NodeType.Or

            val childEvents = remainingEvents.take(groupAmount)
            remainingEvents.dropInPlace(groupAmount)

            val newNode = makeNode(randomNodeType, childEvents)
            remainingEvents.addOne(newNode)
        end while

        remainingEvents.head
    }

    def main(args: Array[String]): Unit = {
        given random: RandomGenerator = new java.util.Random()

        for (_ <- 0 until 10) {
            println(makeRandomTree(10))
        }
    }
}

object Plots {

    def main(args: Array[String]): Unit = {
        given random: RandomGenerator = new java.util.Random()

        val averages = new ListBuffer[Average]()

        for (basicEvents <- 5 to 55 by 5) {
            val nIterations = 50

            var sumRecursive = 0.0
            var sumCutSet = 0.0
            var sumPathSet = 0.0

            for (_ <- 0 until nIterations) {
                val faultTree = RandomTrees.makeRandomTree(basicEvents)
                val (dagTree, probabilities) = Conversion.translateToDagTree(faultTree)
                val dagBasicEvents = minimalcutpathset.getBasicEvents(dagTree)

                val heightRecursive = faulttree.height(faultTree)
                val heightCutSet = minimalcutpathset.height4(dagTree, dagBasicEvents, probabilities)
                val heightPathSet = minimalcutpathset.height5(dagTree, dagBasicEvents, probabilities)

                sumRecursive += heightRecursive
                sumCutSet += heightCutSet
                sumPathSet += heightPathSet
            }

            val averageRecursive = sumRecursive / nIterations
            val averageCutSet = sumCutSet / nIterations
            val averagePathSet = sumPathSet / nIterations

            println(s"#basic events: ${basicEvents}")
            println(s"averageRecursive = ${averageRecursive}")
            println(s"averageCutSet = ${averageCutSet}")
            println(s"averagePathSet = ${averagePathSet}")
            println()

            averages.addOne(Average(basicEvents, averageRecursive, averageCutSet, averagePathSet))
        }

        // TODO save to file?
        Plot3D.draw3d(averages)
        Plot2D.draw2d(averages)
    }

}

case class Average(basicEvents: Int, averageRecursive: Double, averageCutSet: Double, averagePathSet: Double)

object Plot3D {
    import org.jzy3d.colors.Color
    import org.jzy3d.maths.{Coord3d, Range, BoundingBox3d}
    import org.jzy3d.plot3d.primitives.Scatter
    import org.jzy3d.plot3d.rendering.canvas.Quality
    import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
    import org.jzy3d.chart.factories.{AWTChartFactory, IChartFactory}

    def draw3d(averages: IterableOnce[Average]): Unit = {
        val coordinates = new Array[Coord3d](averages.size)
        val colours = new Array[Color](averages.size)

        var i = 0
        for (Average(basicEvents, averageRecursive, averageCutSet, averagePathSet) <- averages) {
            colours(i) = colour(basicEvents)
            coordinates(i) = new Coord3d(averageRecursive, averageCutSet, averagePathSet)
            i += 1
        }

        val scatter = new Scatter(coordinates, colours)

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart = chartFactory.newChart(Quality.Advanced())
        chart.getView.setBoundMode(ViewBoundMode.MANUAL) // Alternatively, MANUAL, AUTO_FIT
        chart.getView.setBoundsManual(new BoundingBox3d(new Range(1, 2), new Range(1, 2), new Range(1, 2)))
        chart.getAxisLayout.setXAxisLabel("Recursion Algorithm Height")
        chart.getAxisLayout.setYAxisLabel("CutSet Algorithm Height")
        chart.getAxisLayout.setZAxisLabel("PathSet Algorithm Height")
        chart.getScene.add(scatter)
        chart.open()
        chart.addMouse()
    }

    def colour(basicEvents: Int): Color = {
        val radians = events2radians(basicEvents)
        new Color(Math.sin(radiansRed(radians)), Math.sin(radiansGreen(radians)), Math.sin(radiansBlue(radians)))
    }

    def radiansRed(radians: Double): Double = radians

    def radiansGreen(radians: Double): Double = radians + (Math.TAU / 3)

    def radiansBlue(radians: Double): Double = radians + (Math.TAU / 3) * 2

    def events2radians(basicEvents: Int): Double = Math.TAU * (basicEvents / 55D)

}

// 2d line plot example: https://doc.jzy3d.org/guide/docs/chapter5.html
object Plot2D {
    import org.jzy3d.colors.Color
    import org.jzy3d.maths.{Coord3d}
    import org.jzy3d.plot3d.primitives.{LineStrip}
    import org.jzy3d.plot3d.rendering.canvas.Quality
    import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
    import org.jzy3d.chart.factories.{AWTChartFactory, IChartFactory}
    import org.jzy3d.maths.{BoundingBox3d}
    import org.jzy3d.plot3d.rendering.legends.overlay.{Legend, OverlayLegendRenderer}

    def draw2d(averages: IterableOnce[Average]): Unit = {
        val lineStripRecursive = new LineStrip()
        val lineStripCutSet = new LineStrip()
        val lineStripPathSet = new LineStrip()

        lineStripRecursive.setWireframeColor(Color.RED)
        lineStripCutSet.setWireframeColor(Color.GREEN)
        lineStripPathSet.setWireframeColor(Color.BLUE)

        for (Average(basicEvents, averageRecursive, averageCutSet, averagePathSet) <- averages) {
            lineStripRecursive.add(new Coord3d(basicEvents, averageRecursive, 0))
            lineStripCutSet.add(new Coord3d(basicEvents, averageCutSet, 0))
            lineStripPathSet.add(new Coord3d(basicEvents, averagePathSet, 0))
        }

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart = chartFactory.newChart(Quality.Advanced())
        chart.getView.setBoundMode(ViewBoundMode.MANUAL) // Alternatively, MANUAL, AUTO_FIT
        chart.getView.setBoundsManual(new BoundingBox3d(0, 60, 0, 2, 0, 0))
        chart.getAxisLayout.setXAxisLabel("# Basic events")
        chart.getAxisLayout.setYAxisLabel("# Approximated height")

        chart.getScene.add(lineStripRecursive)
        chart.getScene.add(lineStripCutSet)
        chart.getScene.add(lineStripPathSet)

        val legendRecursive = new Legend("Recursive", Color.RED)
        val legendCutSet = new Legend("CutSet", Color.GREEN)
        val legendPathSet = new Legend("PathSet", Color.BLUE)
        //TODO how to add these legends?

//        val legendRenderer = new OverlayLegendRenderer(legendRecursive, legendCutSet, legendPathSet)
//        chart.add(legendRecursive)
//        chart.add(legendRenderer)

        chart.open()
        chart.addMouse()
    }

}

