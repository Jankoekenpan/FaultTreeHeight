package benchmark

import java.util.random.RandomGenerator
import faulttree.FaultTree
import faulttree.FaultTree.*

import scala.collection.mutable.ListBuffer

enum NodeType:
    case Basic, And, Or

object NodeType:
    val Values: Seq[NodeType] = Seq(NodeType.Basic, NodeType.And, NodeType.Or)

def randomProbability()(using random: RandomGenerator): Double =
    random.nextDouble(Double.MinPositiveValue, 1D)

object RandomTrees {

    //TODO test this method using ScalaTest property-based testing.
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

        val points = new ListBuffer[Coordinate]()
        val averages = new ListBuffer[Average]()

        val (chart, scatter) = Plot3D.drawScatter()

        val csvOutput = CSVOutput.newFile()
        CSVOutput.printHeader(csvOutput)

        for (basicEvents <- 5 to 100/*TODO 100*/ by 5) {
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

                val point = Coordinate(basicEvents, heightRecursive, heightCutSet, heightPathSet)

                points.addOne(point)
                Plot3D.plotHeights(chart, scatter, points)
                CSVOutput.printData(csvOutput, point)

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

        //Plot3D.draw3d(points)
        Plot2D.draw2d(averages)
    }

}

case class Coordinate(basicEvents: Int, heightRecursive: Double, heightCutSet: Double, heightPathSet: Double)
case class Average(basicEvents: Int, averageRecursive: Double, averageCutSet: Double, averagePathSet: Double)

object Plot3D {
    import org.jzy3d.chart.Chart
    import org.jzy3d.chart.factories.{AWTChartFactory, IChartFactory}
    import org.jzy3d.colors.Color
    import org.jzy3d.maths.{Coord3d, Range, BoundingBox3d}
    import org.jzy3d.plot3d.primitives.Scatter
    import org.jzy3d.plot3d.rendering.canvas.Quality
    import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
    import scala.jdk.CollectionConverters.given

    //one-shot
    def draw3d(averages: scala.collection.Seq[Coordinate]): Unit = {
        val coordinates = new Array[Coord3d](averages.size)
        val colours = new Array[Color](averages.size)

        var i = 0
        for (Coordinate(basicEvents, heightRecursive, heightCutSet, heightPathSet) <- averages) {
            colours(i) = colour(basicEvents)
            coordinates(i) = new Coord3d(heightRecursive, heightCutSet, heightPathSet)
            i += 1
        }

        val scatter = new Scatter(coordinates, colours)

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart: Chart = chartFactory.newChart(Quality.Advanced())
        chart.getView.setBoundMode(ViewBoundMode.AUTO_FIT) //another option: MANUAL
        //chart.getView.setBoundsManual(new BoundingBox3d(new Range(1, 2), new Range(1, 2), new Range(1, 2)))
        chart.getAxisLayout.setXAxisLabel("Recursion Algorithm Height")
        chart.getAxisLayout.setYAxisLabel("CutSet Algorithm Height")
        chart.getAxisLayout.setZAxisLabel("PathSet Algorithm Height")
        chart.getScene.add(scatter)
        chart.open()
        chart.render()
        chart.addMouse()
    }

    // continuous
    def drawScatter(): (Chart, Scatter) = {
        val scatter = new Scatter()

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart: Chart = chartFactory.newChart(Quality.Advanced())
        chart.getView.setBoundMode(ViewBoundMode.AUTO_FIT)
        chart.getAxisLayout.setXAxisLabel("Recursion Algorithm Height")
        chart.getAxisLayout.setYAxisLabel("CutSet Algorithm Height")
        chart.getAxisLayout.setZAxisLabel("PathSet Algorithm Height")
        chart.getScene.add(scatter)
        chart.open()
        chart.render()
        chart.addMouse()

        (chart, scatter)
    }

    def plotHeights(chart: Chart, scatter: Scatter, points: scala.collection.Seq[Coordinate]): Unit = {
        scatter.setData(points.map(coord).asJava)
        scatter.setColors(points.map(colour).toArray)
        chart.getView.updateBounds()
        chart.render()
    }

    // helpers
    def coord(coordinate: Coordinate): Coord3d = coordinate match
        case Coordinate(basicEvents, heightRecursive, heightCutSet, heightPathSet) => new Coord3d(heightRecursive, heightCutSet, heightPathSet)

    def colour(coordinate: Coordinate): Color = colour(coordinate.basicEvents)

    def colour(basicEvents: Int): Color = {
        val radians = events2radians(basicEvents)
        new Color(Math.sin(radiansRed(radians)), Math.sin(radiansGreen(radians)), Math.sin(radiansBlue(radians)))
    }

    def radiansRed(radians: Double): Double = radians

    def radiansGreen(radians: Double): Double = radians + (Math.TAU / 3)

    def radiansBlue(radians: Double): Double = radians + (Math.TAU / 3) * 2

    def events2radians(basicEvents: Int): Double = Math.TAU * (basicEvents / 100D)

}

object Plot2D {
    import java.awt.Font
    import org.jzy3d.chart.AWTChart
    import org.jzy3d.colors.Color
    import org.jzy3d.plot3d.rendering.canvas.Quality
    import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
    import org.jzy3d.chart.factories.{AWTChartFactory, IChartFactory}
    import org.jzy3d.maths.BoundingBox3d
    import org.jzy3d.plot3d.rendering.legends.overlay.{Legend, OverlayLegendRenderer}
    import org.jzy3d.plot2d.primitives.LineSerie2d

    // Code adopted from: https://github.com/jzy3d/jzy3d-api/blob/master/jzy3d-tutorials/src/main/java/org/jzy3d/demos/chart2d/Line2D_DemoAWT.java
    def draw2d(averages: IterableOnce[Average]): Unit = {
        val lineRecursive = new LineSerie2d("Recursive");
        val lineCutSet = new LineSerie2d("CutSet")
        val linePathSet = new LineSerie2d("PathSet")

        lineRecursive.setColor(Color.RED)
        lineCutSet.setColor(Color.GREEN)
        linePathSet.setColor(Color.BLUE)

        for (Average(basicEvents, averageRecursive, averageCutSet, averagePathSet) <- averages) {
            lineRecursive.add(basicEvents, averageRecursive)
            lineCutSet.add(basicEvents, averageCutSet)
            linePathSet.add(basicEvents, averagePathSet)
        }

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart = chartFactory.newChart(Quality.Advanced()).asInstanceOf[AWTChart]
        chart.getView.setBoundMode(ViewBoundMode.MANUAL)
        chart.getView.setBoundsManual(new BoundingBox3d(0, 100, 0, 2, 0, 0))
        chart.getAxisLayout.setXAxisLabel("# Basic events")
        chart.getAxisLayout.setYAxisLabel("Approximated height")

        chart.add(lineRecursive)
        chart.add(lineCutSet)
        chart.add(linePathSet)

        val legendRecursive = new Legend(lineRecursive.getName, lineRecursive.getColor)
        val legendCutSet = new Legend(lineCutSet.getName, lineCutSet.getColor)
        val legendPathSet = new Legend(linePathSet.getName, linePathSet.getColor)
        val legendRenderer = new OverlayLegendRenderer(legendRecursive, legendCutSet, legendPathSet)
        val layout = legendRenderer.getLayout

        layout.getMargin.setWidth(10)
        layout.getMargin.setHeight(10)
        layout.setBackgroundColor(Color.WHITE)
        layout.setFont(new Font("Helvetica", Font.PLAIN, 11))

        chart.addRenderer(legendRenderer)

        chart.view2d()
        chart.open()
    }

}

object CSVOutput {
    import java.nio.file.{Files, Path, StandardOpenOption}

    private val outFile = "points.csv"

    def newFile(): Path = {
        Files.createFile(Path.of(outFile))
    }

    def printHeader(file: Path): Unit = {
        val line = "# Basic events,Height (recursive algorithm),Height (cut set algorithm),Height (path set algorithm)\r\n"
        writeString(file, line)
    }

    def printData(file: Path, point: Coordinate): Unit = point match
        case Coordinate(events, recursive, cutset, pathset) =>
            writeString(file, s""""$events","$recursive","$cutset","$pathset"\r\n""")

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }

}