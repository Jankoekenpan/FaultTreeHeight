package benchmark

import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.{switch, tailrec}
import benchmark.Setup.{Branching, Recipe, other, ppDecisionTree, ppFaultTree}
import faulttree.FaultTree

import scala.collection.immutable.IntMap

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
        val line = "Fault Tree,height (enumeration algorithm),height (vector operations algorithm),height (cutset/pathset algorithm),height (algorithm 6),height (algorithm 4),height (algorithm 5),approximation ratio vector algorithm,approxmiation ratio cutset/pathset algorithm,algorithm 6,algorithm 4, algorithm 5\n"
        print(line)
        writeString(file, line)
    }

    def printRatio(file: Path, recipe: Recipe): Unit =
        printRatio(file, makeAlternatingFaultTree(recipe))

    def printRatio(file: Path, faultTree: FaultTree): Unit = {
        val decisionTree = translateToDecisionTree(faultTree)
        val dagTree = translateToDagTree(faultTree)

        println(s"DEBUG tree faulttree = $faultTree")
        println(s"DEBUG decisiontree = $decisionTree")
        println(s"DEBUG dag faulttree = $dagTree")
        val millisBefore = System.currentTimeMillis();

        val heightFaultTree = faulttree.height(faultTree)
        val heightDecisionTree = decisiontree.height.tupled(decisionTree)
        val heightDagTree = minimalcutpathset.height.tupled(dagTree)
        val heightAlgo6 = minimalcutpathset.height6.tupled(dagTree)
        val heightAlgo4 = minimalcutpathset.height4(dagTree._1)
        val heightAlgo5 = minimalcutpathset.height5(dagTree._1)

        val millisAfter = System.currentTimeMillis()

        val differenceMillis = millisAfter - millisBefore
        println(s"computation took ${differenceMillis / 1000} seconds.")

        val line = s""""$faultTree","$heightDecisionTree","$heightFaultTree","$heightDagTree","$heightAlgo6","$heightAlgo4","$heightAlgo5","${ratio(heightFaultTree, heightDecisionTree)}","${ratio(heightDagTree, heightDecisionTree)}","${ratio(heightAlgo6, heightDecisionTree)}","${ratio(heightAlgo4, heightDecisionTree)}","${ratio(heightAlgo5, heightDecisionTree)}"\n""".stripMargin
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

    def translateToDagTree(tree: faulttree.FaultTree): (minimalcutpathset.FaultTree, IntMap[Double]) = {
        val eventsBuilder = scala.collection.immutable.Map.newBuilder[Int, minimalcutpathset.TreeNode]
        val probabilities = IntMap.newBuilder[Double]

        def translateToTree(tree: faulttree.FaultTree): minimalcutpathset.TreeNode = {
            val treeNode = tree match
                case faulttree.FaultTree.BasicEvent(id, p) =>
                    probabilities.addOne(id, p)
                    minimalcutpathset.TreeNode.BasicEvent(id, p)
                case faulttree.FaultTree.AndEvent(id, children) =>
                    children.foreach(translateToTree)
                    minimalcutpathset.TreeNode.Combination(
                        id,
                        minimalcutpathset.Gate.And,
                        children.map(_.event).toSet
                    )
                case faulttree.FaultTree.OrEvent(id, children) =>
                    children.foreach(translateToTree)
                    minimalcutpathset.TreeNode.Combination(
                        id,
                        minimalcutpathset.Gate.Or,
                        children.map(_.event).toSet
                    )
            eventsBuilder.addOne(tree.event, treeNode)
            treeNode
        }

        translateToTree(tree)

        (minimalcutpathset.FaultTree(tree.event, eventsBuilder.result()), probabilities.result())
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

    def debug[A](a: A): A = {
        println(s"DEBUG a=${a}")
        a
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
