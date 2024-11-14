package benchmark

import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.{switch, tailrec}
import benchmark.Setup.{Branching, Recipe, other, ppDecisionTree, ppFaultTree}
import faulttree.FaultTree

object ApproximationRatio {

    val jur = new java.util.Random()
    val outFile = "ratios.csv"

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }

    def childWidth(depth: Int): Int = (depth: @switch) match
        case 2 => jur.nextInt(2, 11)
        case 3 => jur.nextInt(2, 4)
        case 4 => jur.nextInt(2, 3)

    def main(args: Array[String]): Unit = {
//        val randomTree = makeAlternatingFaultTree(Recipe(
//            depth = 3,
//            branching = Seq(Branching.And),
//            childIsBasicProbability = 0,
//            branchingWidth = 3,
//            probabilityOf = id => Math.random()
//        ))

//        println(ppFaultTree(faulttree.reproTree))
//        println(ppDecisionTree.tupled(translateToDecisionTree(faulttree.reproTree)))
//        val (dagTree, probabilities) = translateToDagTree(faulttree.reproTree)
//        println(s"DEBUG dagtree = $dagTree")
//        println(s"DEBUG height = ${minimalcutpathset.height(dagTree, probabilities)}")

        val file = Files.createFile(Path.of(outFile))
        printHeader(file)

        printRatio(file, faulttree.reproTree)

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

//        printRatio(file, decisiontree.faultTree)
    }

    def printHeader(file: Path): Unit = {
        val line = "Fault Tree,height (algorithm 1 - enumeration),height (algorithm 2 - vector),height (algorithm 4 - cutset),height (algorithm 5 - pathset),height (algorithm 7 - vector algorithm 2),height (algorithm 8 - binary decision tree algorithm),approximation ratio algorithm 2,approximation ratio algorithm 4,approximation ratio algorithm 5,approximation ratio algorithm 7,approximation ratio algorithm 8\n"
        print(line)
        writeString(file, line)
    }

    def printRatio(file: Path, recipe: Recipe): Unit =
        printRatio(file, makeAlternatingFaultTree(recipe))

    def printRatio(file: Path, faultTree: FaultTree): Unit = {
        val decisionTree = Conversion.translateToBooleanFormula(faultTree)
        val dagTree = Conversion.translateToDagTree(faultTree)

        println(s"DEBUG tree faulttree = $faultTree")
        println(s"DEBUG decisiontree = $decisionTree")
        println(s"DEBUG dag faulttree = $dagTree")
        val millisBefore = System.currentTimeMillis();

        val heightDecisionTree = decisiontree.height.tupled(decisionTree)           // enumeration algorithm
        val heightFaultTree = faulttree.height(faultTree)                           // recursive algorithm 1
//        val heightDagTree = minimalcutpathset.height.tupled(dagTree)                // UNUSED: cutset/pathset algorithm
//        val heightAlgo6 = minimalcutpathset.height6.tupled(dagTree)                 // UNUSED: cutset algorithm with problem
        val heightAlgo4 = minimalcutpathset.height4(dagTree._1)                     // cutset algorithm
        val heightAlgo5 = minimalcutpathset.height5(dagTree._1)                     // pathset algorithm
        val heightAlgo7 = faulttree.height7(faultTree)                              // recursive algorithm 2
        val (bdt, heightAlgo8) = decisiontree.algorithm8(dagTree._1, dagTree._2)    // dag-to-binary_decision_tree algorithm

        val millisAfter = System.currentTimeMillis()

        val differenceMillis = millisAfter - millisBefore
        println(s"computation took ${differenceMillis / 1000} seconds.")

        val line = s""""$faultTree","$heightDecisionTree","$heightFaultTree","$heightAlgo4","$heightAlgo5","$heightAlgo7","$heightAlgo8","${ratio(heightFaultTree, heightDecisionTree)}","${ratio(heightAlgo4, heightDecisionTree)}","${ratio(heightAlgo5, heightDecisionTree)}","${ratio(heightAlgo7, heightDecisionTree)}","${ratio(heightAlgo8, heightDecisionTree)}"\n""".stripMargin
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
