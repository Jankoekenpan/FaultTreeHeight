package benchmark

import java.util.random.RandomGenerator
import faulttree.FaultTree
import faulttree.FaultTree.*

enum NodeType:
    case Basic, And, Or

def randomProbability()(using random: RandomGenerator): Double =
    random.nextDouble(Double.MinPositiveValue, 1D)

object RandomTrees {

    //TODO test this method using ScalaCheck property-based testing.
    def makeRandomTree(basicEvents: Int)(using random: RandomGenerator): FaultTree = {
        var id = 0

        def nextId(): Int = {
            val oldId = id
            id += 1
            oldId
        }

        val basicEventsNodes: Seq[FaultTree.BasicEvent] = for _ <- 0 until basicEvents yield FaultTree.BasicEvent(nextId(), randomProbability())

        // Queue of events to be processed: start with only basic events.
        var remainingEvents: IndexedSeq[FaultTree] = IndexedSeq.from(basicEventsNodes)

        def makeNode(nodeType: NodeType, children: IterableOnce[FaultTree]): FaultTree = nodeType match
            case NodeType.And => FaultTree.AndEvent(nextId(), Seq.from(children))
            case NodeType.Or => FaultTree.OrEvent(nextId(), Seq.from(children))

        // While there are 2 or more events in the queue:
        while remainingEvents.sizeIs > 1 do
            // Group them together as children of a new parent node.
            val groupAmount = 2 + random.nextInt(remainingEvents.size - 1)
            val randomNodeType = if random.nextBoolean() then NodeType.And else NodeType.Or

            // Cut children from queue.
            val childEvents = remainingEvents.take(groupAmount)
            remainingEvents = remainingEvents.drop(groupAmount)

            // Insert new parent node at random place in the queue.
            val newNode = makeNode(randomNodeType, childEvents)
            val newNodeIndex = random.nextInt(remainingEvents.size + 1)
            remainingEvents = remainingEvents.patch(newNodeIndex, Seq(newNode), 0)
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

//        val points = new ListBuffer[Coordinate]() // only used for 3d plot
//        val averages = new ListBuffer[Average]()            //TODO might not need this.
//        val averageTimes = new ListBuffer[AverageTime]()  //TODO might not need this.

//        val (chart, scatter) = Plot3D.drawScatter()
        val (heightsChart, heightLines) = Plot2D.drawHeights()
        val (timesChart, timesLines) = Plot2D.drawTimes()

        val csvOutput = CSVOutput.newDataFile()
        CSVOutput.printDataHeader(csvOutput)
        val csvTimeOutput = CSVOutput.newTimingsFile()
        CSVOutput.printTimingsHeader(csvTimeOutput)

        val nIterations = 50
        for (basicEvents <- 85/*TODO 5*/ to 85/*TODO 100*/ by 5) {

            var sumRecursive1 = 0.0
            var sumCutSet = 0.0
            var sumPathSet = 0.0
            var sumRecursive2 = 0.0

            var sumTimeRecursive1_ns = 0L
            var sumTimeCutSet_ns = 0L
            var sumTimePathSet_ns = 0L
            var sumTimeRecursive2_ns = 0L

            for (it <- 1 to nIterations) {
                println(s"Iteration: ${it}")

                val faultTree = RandomTrees.makeRandomTree(basicEvents)
                val (dagTree, probabilities) = Conversion.translateToDagTree(faultTree)
                val dagBasicEvents = minimalcutpathset.getBasicEvents(dagTree)

//                println("Calculating cut sets...")
                val minimalCutSets = minimalcutpathset.minimalCutSets(dagTree)(dagBasicEvents)
//                println("Calculating path sets...")
                val minimalPathSets = minimalcutpathset.minimalPathSets(dagTree)(dagBasicEvents)

                println("Calculate height using Recursive algorithm 1...")
                val time_1 = System.nanoTime()
                val heightRecursive1 = faulttree.height(faultTree)
                val time1_end = System.nanoTime()
                println("Calculate height using Recursive algorithm 2...")
                val time_4 = System.nanoTime()
                val heightRecursive2 = faulttree.height7(faultTree)
                val time4_end = System.nanoTime()
                println("Calculate height using CutSet algorithm...")
                val time_2 = System.nanoTime()
                val heightCutSet = minimalcutpathset.algorithm4(minimalCutSets, probabilities)._2
                val time2_end = System.nanoTime()
                println("Calculate height using PathSet algorithm...")
                val time_3 = System.nanoTime()
                val heightPathSet = minimalcutpathset.algorithm5(minimalPathSets, probabilities)._2
                val time3_end = System.nanoTime()
//                val time_5 = System.nanoTime()
                println("Finished height calculations!")

                val recursive1_ns = time1_end - time_1
                val cutset_ns = time2_end - time_2
                val pathset_ns = time3_end - time_3
                val recursive2_ns = time4_end - time_4

                val point = Coordinate(basicEvents, heightRecursive1, heightCutSet, heightPathSet, heightRecursive2)
                val time = Time(basicEvents, recursive1_ns, cutset_ns, pathset_ns, recursive2_ns)

//                points.addOne(point)  // only for 3d plot
//                Plot3D.plotHeights(chart, scatter, points)

                CSVOutput.printData(csvOutput, point)
                CSVOutput.printTimings(csvTimeOutput, time)

                sumRecursive1 += heightRecursive1
                sumCutSet += heightCutSet
                sumPathSet += heightPathSet
                sumRecursive2 += heightRecursive2

                sumTimeRecursive1_ns += recursive1_ns
                sumTimeCutSet_ns += cutset_ns
                sumTimePathSet_ns += pathset_ns
                sumTimeRecursive2_ns += recursive2_ns
            }

            val averageRecursive1 = sumRecursive1 / nIterations
            val averageCutSet = sumCutSet / nIterations
            val averagePathSet = sumPathSet / nIterations
            val averageRecursive2 = sumRecursive2 / nIterations

            val averageTimeRecursive1_ms = sumTimeRecursive1_ns / nIterations / 10_000_000D
            val averageTimeCutSet_ms = sumTimeCutSet_ns / nIterations / 10_000_000D
            val averageTimePatSet_ms = sumTimePathSet_ns / nIterations / 10_000_000D
            val averageTimeRecursive2_ms = sumTimeRecursive2_ns / nIterations / 10_000_000D

            println(s"#basic events: ${basicEvents}")
            println(s"averageRecursive1 = ${averageRecursive1}")
            println(s"averageCutSet = ${averageCutSet}")
            println(s"averagePathSet = ${averagePathSet}")
            println(s"averageRecursive2 = ${averageRecursive2}")
            println()
            println(s"averageTimeRecursive1 = ${averageTimeRecursive1_ms} ms")
            println(s"averageTimeCutSet = ${averageTimeCutSet_ms} ms")
            println(s"averageTimePathSet = ${averageTimePatSet_ms} ms")
            println(s"averageTimeRecursive2 = ${averageTimeRecursive2_ms} ms")
            println()
            println()

            val averageHeights = Average(basicEvents, averageRecursive1, averageCutSet, averagePathSet, averageRecursive2)
//            averages.addOne(averageHeights)     //TODO might not need this.
            val averageTime = AverageTime(basicEvents, averageTimeRecursive1_ms, averageTimeCutSet_ms, averageTimePatSet_ms, averageTimeRecursive2_ms)
//            averageTimes.addOne(averageTime)  //TODO might not need this.

            Plot2D.addHeights(heightsChart, heightLines, averageHeights)
            Plot2D.addTimes(timesChart, timesLines, averageTime)
        }

        //Plot3D.draw3d(points)
        //Plot2D.draw2d(averages)
    }

}

case class Coordinate(basicEvents: Int, heightRecursive1: Double, heightCutSet: Double, heightPathSet: Double, heightRecursive2: Double)
case class Time(basicEvents: Int, timeRecursive1_ns: Long, timeCutSet_ns: Long, timePathSet_ns: Long, timeRecursive2_ns: Long)
case class Average(basicEvents: Int, averageRecursive1: Double, averageCutSet: Double, averagePathSet: Double, averageRecursive2: Double)
case class AverageTime(basicEvents: Int, averageTimeRecursive1_ms: Double, averageTimeCutSet_ms: Double, averageTimePathSet_ms: Double, averageTimeRecursive2_ms: Double)

@Deprecated
object Plot3D {
    import org.jzy3d.chart.Chart
    import org.jzy3d.chart.factories.{AWTChartFactory, IChartFactory}
    import org.jzy3d.colors.Color
    import org.jzy3d.maths.{Coord3d, Range, BoundingBox3d}
    import org.jzy3d.plot3d.primitives.Scatter
    import org.jzy3d.plot3d.rendering.canvas.Quality
    import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
    import scala.jdk.CollectionConverters.given

    // one-shot
    def draw3d(averages: scala.collection.Seq[Coordinate]): Unit = {
        val coordinates = new Array[Coord3d](averages.size)
        val colours = new Array[Color](averages.size)

        var i = 0
        for (Coordinate(basicEvents, heightRecursive1, heightCutSet, heightPathSet, heightRecursive2) <- averages) {
            colours(i) = colour(basicEvents)
            coordinates(i) = new Coord3d(heightRecursive1, heightCutSet, heightPathSet)
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
        case Coordinate(basicEvents, heightRecursive1, heightCutSet, heightPathSet, heightRecursive2) => new Coord3d(heightRecursive1, heightCutSet, heightPathSet)

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
    import org.jzy3d.chart.{AWTChart, Chart}
    import org.jzy3d.colors.Color
    import org.jzy3d.plot3d.rendering.canvas.Quality
    import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode
    import org.jzy3d.chart.factories.{AWTChartFactory, IChartFactory}
    import org.jzy3d.maths.BoundingBox3d
    import org.jzy3d.plot3d.rendering.legends.overlay.{Legend, OverlayLegendRenderer}
    import org.jzy3d.plot2d.primitives.LineSerie2d

    // Code adapted from: https://github.com/jzy3d/jzy3d-api/blob/master/jzy3d-tutorials/src/main/java/org/jzy3d/demos/chart2d/Line2D_DemoAWT.java
    // one-shot
    def draw2d(averages: IterableOnce[Average]): Unit = {
        val lineRecursive1 = new LineSerie2d("Recursive 1")
        val lineCutSet = new LineSerie2d("CutSet")
        val linePathSet = new LineSerie2d("PathSet")
        val lineRecursive2 = new LineSerie2d("Recursive 2")

        lineRecursive1.setColor(Color.RED)
        lineCutSet.setColor(Color.GREEN)
        linePathSet.setColor(Color.BLUE)
        lineRecursive2.setColor(new Color(255, 0, 255))

        for (Average(basicEvents, averageRecursive1, averageCutSet, averagePathSet, averageRecursive2) <- averages) {
            lineRecursive1.add(basicEvents, averageRecursive1)
            lineCutSet.add(basicEvents, averageCutSet)
            linePathSet.add(basicEvents, averagePathSet)
            lineRecursive2.add(basicEvents, averageRecursive2)
        }

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart = chartFactory.newChart(Quality.Advanced()).asInstanceOf[AWTChart]
        chart.getView.setBoundMode(ViewBoundMode.MANUAL)
        chart.getView.setBoundsManual(new BoundingBox3d(0, 100, 0, 2, 0, 0))
        chart.getAxisLayout.setXAxisLabel("# Basic events")
        chart.getAxisLayout.setYAxisLabel("Average approximated height")    // 50 samples

        chart.add(lineRecursive1)
        chart.add(lineCutSet)
        chart.add(linePathSet)
        chart.add(lineRecursive2)

        val legendRecursive1 = new Legend(lineRecursive1.getName, lineRecursive1.getColor)
        val legendCutSet = new Legend(lineCutSet.getName, lineCutSet.getColor)
        val legendPathSet = new Legend(linePathSet.getName, linePathSet.getColor)
        val legendRecursive2 = new Legend(lineRecursive2.getName, lineRecursive2.getColor)
        val legendRenderer = new OverlayLegendRenderer(legendRecursive1, legendCutSet, legendPathSet, legendRecursive2)
        val layout = legendRenderer.getLayout

        layout.getMargin.setWidth(10)
        layout.getMargin.setHeight(10)
        layout.setBackgroundColor(Color.WHITE)
        layout.setFont(new Font("Helvetica", Font.PLAIN, 11))

        chart.addRenderer(legendRenderer)

        chart.view2d()
        chart.open()
    }

    // continuous
    case class Lines(recursive1: LineSerie2d, cutset: LineSerie2d, pathset: LineSerie2d, recursive2: LineSerie2d)

    def drawHeights(): (Chart, Lines) = {
        val lineRecursive1 = new LineSerie2d("Recursive 1")
        val lineCutSet = new LineSerie2d("CutSet")
        val linePathSet = new LineSerie2d("PathSet")
        val lineRecursive2 = new LineSerie2d("Recursive 2")

        lineRecursive1.setColor(Color.RED)
        lineCutSet.setColor(Color.GREEN)
        linePathSet.setColor(Color.BLUE)
        lineRecursive2.setColor(new Color(255, 0, 255))

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart = chartFactory.newChart(Quality.Advanced()).asInstanceOf[AWTChart]
        chart.getView.setBoundMode(ViewBoundMode.MANUAL)
        chart.getView.setBoundsManual(new BoundingBox3d(0, 100, 0, 2, 0, 0))
        chart.getAxisLayout.setXAxisLabel("# Basic events")
        chart.getAxisLayout.setYAxisLabel("Average approximated height") // 50 samples

        chart.add(lineRecursive1)
        chart.add(lineCutSet)
        chart.add(linePathSet)
        chart.add(lineRecursive2)

        val legendRecursive1 = new Legend(lineRecursive1.getName, lineRecursive1.getColor)
        val legendCutSet = new Legend(lineCutSet.getName, lineCutSet.getColor)
        val legendPathSet = new Legend(linePathSet.getName, linePathSet.getColor)
        val legendRecursive2 = new Legend(lineRecursive2.getName, lineRecursive2.getColor)
        val legendRenderer = new OverlayLegendRenderer(legendRecursive1, legendCutSet, legendPathSet, legendRecursive2)
        val layout = legendRenderer.getLayout

        layout.getMargin.setWidth(10)
        layout.getMargin.setHeight(10)
        layout.setBackgroundColor(Color.WHITE)
        layout.setFont(new Font("Helvetica", Font.PLAIN, 11))

        chart.addRenderer(legendRenderer)

        chart.view2d()
        chart.open()

        (chart, Lines(lineRecursive1, lineCutSet, linePathSet, lineRecursive2))
    }

    def addHeights(chart: Chart, lines: Lines, average: Average): Unit = (lines, average) match {
        case (Lines(l1, l2, l3, l4), Average(be, h1, h2, h3, h4)) =>
            l1.add(be, h1)
            l2.add(be, h2)
            l3.add(be, h3)
            l4.add(be, h4)
            chart.render()
    }

    def drawTimes(): (Chart, Lines) = {
        val lineRecursive1 = new LineSerie2d("Recursive 1")
        val lineCutSet = new LineSerie2d("CutSet")
        val linePathSet = new LineSerie2d("PathSet")
        val lineRecursive2 = new LineSerie2d("Recursive 2")

        lineRecursive1.setColor(Color.RED)
        lineCutSet.setColor(Color.GREEN)
        linePathSet.setColor(Color.BLUE)
        lineRecursive2.setColor(new Color(255, 0, 255))

        val chartFactory: IChartFactory = new AWTChartFactory()
        val chart = chartFactory.newChart(Quality.Advanced()).asInstanceOf[AWTChart]
        chart.getView.setBoundMode(ViewBoundMode.AUTO_FIT)
        chart.getAxisLayout.setXAxisLabel("# Basic events")
        chart.getAxisLayout.setYAxisLabel("Average execution times (ms)") // 50 samples

        chart.add(lineRecursive1)
        chart.add(lineCutSet)
        chart.add(linePathSet)
        chart.add(lineRecursive2)

        val legendRecursive1 = new Legend(lineRecursive1.getName, lineRecursive1.getColor)
        val legendCutSet = new Legend(lineCutSet.getName, lineCutSet.getColor)
        val legendPathSet = new Legend(linePathSet.getName, linePathSet.getColor)
        val legendRecursive2 = new Legend(lineRecursive2.getName, lineRecursive2.getColor)
        val legendRenderer = new OverlayLegendRenderer(legendRecursive1, legendCutSet, legendPathSet, legendRecursive2)
        val layout = legendRenderer.getLayout

        layout.getMargin.setWidth(10)
        layout.getMargin.setHeight(10)
        layout.setBackgroundColor(Color.WHITE)
        layout.setFont(new Font("Helvetica", Font.PLAIN, 11))

        chart.addRenderer(legendRenderer)

        chart.view2d()
        chart.open()

        (chart, Lines(lineRecursive1, lineCutSet, linePathSet, lineRecursive2))
    }

    def addTimes(chart: Chart, lines: Lines, average: AverageTime): Unit = (lines, average) match {
        case (Lines(l1, l2, l3, l4), AverageTime(be, t1, t2, t3, t4)) =>
            l1.add(be, t1)
            l2.add(be, t2)
            l3.add(be, t3)
            l4.add(be, t4)
            chart.getView.updateBounds()
            chart.render()
    }

}

object CSVOutput {
    import java.nio.file.{Files, Path, StandardOpenOption}

    private val outFile = "points.csv"
    private val timingsFile = "timings.csv"

    def newDataFile(): Path = {
        Files.createFile(Path.of(outFile))
    }

    def newTimingsFile(): Path = {
        Files.createFile(Path.of(timingsFile))
    }

    def printDataHeader(file: Path): Unit = {
        val line = "# Basic events,Height (recursive algorithm 1),Height (cut set algorithm),Height (path set algorithm),Height (recursive algorithm 2)\r\n"
        writeString(file, line)
    }

    def printTimingsHeader(file: Path): Unit = {
        val line = "# Basic events,Execution time (recursive algorithm 1) (ns),Execution time (cut set algorithm) (ns),Execution time (path set algorithm) (ns),Execution time (recursive algorithm 2) (ns)\r\n"
        writeString(file, line)
    }

    def printData(file: Path, point: Coordinate): Unit = point match
        case Coordinate(events, recursive1, cutset, pathset, recursive2) =>
            writeString(file, s""""$events","$recursive1","$cutset","$pathset","$recursive2"\r\n""")

    def printTimings(file: Path, timings: Time): Unit = timings match
        case Time(events, recursive1_ns, cutset_ns, pathset_ns, recursive2_ns) =>
            writeString(file, s""""$events","${recursive1_ns}","${cutset_ns}","${pathset_ns}","${recursive2_ns}"\r\n""")

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }

}