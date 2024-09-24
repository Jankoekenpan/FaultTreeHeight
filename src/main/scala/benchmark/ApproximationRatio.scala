package benchmark

import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.{switch, tailrec}
import benchmark.Setup.{Branching, Recipe, other, ppDecisionTree, ppFaultTree}

object ApproximationRatio {

    val jur = new java.util.Random()
    val outFile = "ratios.csv"

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }

    def childWidth(depth: Int): Int = (depth: @switch) match
        case 2 => jur.nextInt(2, 11)
        case 3 => jur.nextInt(2, 4)
        case 4 => 2

    def main(args: Array[String]): Unit = {
        val randomTree = makeAlternatingFaultTree(Recipe(
            depth = 3,
            branching = Seq(Branching.And),
            childIsBasicProbability = 0,
            branchingWidth = 3,
            probabilityOf = id => Math.random()
        ))

        println(ppFaultTree(randomTree))
        println(ppDecisionTree.tupled(translateToDecisionTree(randomTree)))

        val file = Files.createFile(Path.of(outFile))

        for (b <- Setup.Branching.values; i <- 1 to 100) {
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
            printRatio(file, recipe(2, b))
        }

        for (b <- Setup.Branching.values; i <- 1 to 100) {
            printRatio(file, recipe(3, b))
            printRatio(file, recipe(3, b))
            //recipe of depth 3 and 4 children per vertex is already problematic for the enumeration algorithm
        }

        for (b <- Setup.Branching.values; i <- 1 to 100) {
            printRatio(file, recipe(4, b))
            //recipe of depth 4 and 3 children per vertex is already problematic for the enumeration algorithm
        }
    }

    def printRatio(file: Path, recipe: Setup.Recipe): Unit = {
        val faultTree = makeAlternatingFaultTree(recipe)
        val decisionTree = translateToDecisionTree(faultTree)

        val heightFaultTree = faulttree.height(faultTree)
        val heightDecisionTree = decisiontree.height.tupled(decisionTree)

        val line = s""""$faultTree","$heightDecisionTree","$heightFaultTree","${ratio(heightFaultTree, heightDecisionTree)}"\n"""
        print(line)
        writeString(file, line)
    }

    def ratio(approximation: Double, realValue: Double): Double = approximation / realValue

    def recipe(depth: Int, startNode: Setup.Branching): Setup.Recipe = Setup.Recipe(
        depth = depth,
        branching = Seq(startNode),
        childIsBasicProbability = 0.1,
        branchingWidth = depth,
        probabilityOf = id => random()
    )

    @tailrec
    def random(): Double = {
        val r = Math.random()
        if r == 0 then random() else r
    }

    def translateToDecisionTree(faultTree: faulttree.FaultTree): (decisiontree.BooleanFormula, Seq[Double]) = {
        val probabilities = Seq.newBuilder[Double]
        var curId = 0;

        def nextId(): Int = {
            val id = curId
            curId += 1
            id
        }

        def matchTree(faultTree: faulttree.FaultTree): decisiontree.BooleanFormula = {
            faultTree match
                case faulttree.FaultTree.BasicEvent(_, p) =>
                    probabilities.addOne(p)
                    decisiontree.BooleanFormula.Variable(nextId())
                case faulttree.FaultTree.AndEvent(_, children) =>
                    Setup.createBalancedAnd(children.map(matchTree), nextId)
                case faulttree.FaultTree.OrEvent(_, children) =>
                    Setup.createBalancedOr(children.map(matchTree), nextId)
        }

        (matchTree(faultTree), probabilities.result())
    }

    def makeAlternatingFaultTree(recipe: Recipe): faulttree.FaultTree = {
        val idGen = new AtomicInteger()

        def makeFaultTree(recipe: Recipe): faulttree.FaultTree = {
            val id = idGen.getAndIncrement()

            if (recipe.depth == 1) {
                faulttree.FaultTree.BasicEvent(id, recipe.probabilityOf(id))
            } else {
                val branchType = recipe.branching(id % recipe.branching.size)
                val children = for
                    _ <- 0 until childWidth(recipe.branchingWidth)
                yield if Math.random() > recipe.childIsBasicProbability then
                    // child is and-gate or or-gate
                    makeFaultTree(recipe.copy(
                        depth = recipe.depth - 1,
                        branching = Seq(other(branchType))
                    ))
                else
                    // child is basic event
                    val childId = idGen.getAndIncrement()
                    faulttree.FaultTree.BasicEvent(childId, recipe.probabilityOf(childId))

                branchType match
                    case Branching.And => faulttree.FaultTree.AndEvent(id, children)
                    case Branching.Or => faulttree.FaultTree.OrEvent(id, children)
            }
        }

        makeFaultTree(recipe)
    }
}
