package benchmark

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, Setup, State}

import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.{switch, tailrec}
import scala.compiletime.uninitialized

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class MyBenchmark {

    private var decisionTree: decisiontree.BooleanFormula = uninitialized
    private var decisionTreeProbabilities: Seq[Double] = uninitialized
    private var decisionTreeIdLookup: (decisiontree.BooleanFormula, Int) => Boolean = uninitialized

    private var faultTree: faulttree.FaultTree = uninitialized
    private var faultTreeLayers: IArray[Seq[faulttree.FaultTree]] = uninitialized

    private var exampleDecisionTree: decisiontree.BooleanFormula = uninitialized
    private var exampleDecisionTreeProbabilities: Seq[Double] = uninitialized
    private var exampleDecisionTreeLookup: (decisiontree.BooleanFormula, Int) => Boolean = uninitialized

    private var exampleFaultTree: faulttree.FaultTree = uninitialized
    private var exampleFaultTreeLayers: IArray[Seq[faulttree.FaultTree]] = uninitialized

    @Setup
    def setup(): Unit = {
        val recipe = Setup.depth4

        val (decisionTree10, decisionTree10Probabilities, decisionTree10IdChecker) = Setup.makeDecisionTree(recipe)
        this.decisionTree = decisionTree10
        this.decisionTreeProbabilities = decisionTree10Probabilities
        this.decisionTreeIdLookup = decisionTree10IdChecker

        this.faultTree = Setup.makeFaultTree(recipe)
        this.faultTreeLayers = faulttree.layers(faultTree)

        this.exampleDecisionTree = decisiontree.BooleanFormula.And(
            decisiontree.BooleanFormula.Or(decisiontree.BooleanFormula.Variable(0), decisiontree.BooleanFormula.Variable(1)),
            decisiontree.BooleanFormula.Or(decisiontree.BooleanFormula.Variable(2), decisiontree.BooleanFormula.Variable(3))
        )
        this.exampleDecisionTreeProbabilities = Seq(1D / 2D, 1D / 3D, 1D / 4D, 1D / 5D)
        this.exampleDecisionTreeLookup = decisiontree.computeLookupBiId()

        this.exampleFaultTree = faulttree.FaultTree.AndEvent('g', Seq(
            faulttree.FaultTree.OrEvent('e', Seq(
                faulttree.FaultTree.BasicEvent('a', 1D/2D),
                faulttree.FaultTree.BasicEvent('b', 1D/3D)
            )),
            faulttree.FaultTree.OrEvent('f', Seq(
                faulttree.FaultTree.BasicEvent('c', 1D/4D),
                faulttree.FaultTree.BasicEvent('d', 1D/5D)
            ))
        ))
        this.exampleFaultTreeLayers = faulttree.layers(exampleFaultTree)
    }

    @Benchmark
    def testHeightDecisionTree(): Double = {
        decisiontree.height(exampleDecisionTree, exampleDecisionTreeProbabilities, exampleDecisionTreeLookup)
    }

    @Benchmark
    def testHeightFaultTree(): Double = {
        faulttree.height(exampleFaultTree, exampleFaultTreeLayers)
    }

    @Benchmark
    def testHeightDecisionTree4(): Double = {
        decisiontree.height(decisionTree, decisionTreeProbabilities, decisionTreeIdLookup)
    }

    @Benchmark
    def testHeightFaultTree4(): Double = {
        faulttree.height(faultTree, faultTreeLayers)
    }


}

object ApproximationRatio {

    val outFile = "ratios.csv"

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }

    def main(args: Array[String]): Unit = {
        val file = Files.createFile(Path.of(outFile))

        for (b <- Setup.Branching.values; i <- 1 to 100) {
            printRatio(file, recipe(2, 2, b))
            printRatio(file, recipe(2, 3, b))
            printRatio(file, recipe(2, 4, b))
            printRatio(file, recipe(2, 5, b))
            printRatio(file, recipe(2, 6, b))
            printRatio(file, recipe(2, 7, b))
            printRatio(file, recipe(2, 8, b))
            printRatio(file, recipe(2, 9, b))
            printRatio(file, recipe(2, 10, b))
        }

        for (b <- Setup.Branching.values; i <- 1 to 100) {
            printRatio(file, recipe(3, 2, b))
            printRatio(file, recipe(3, 3, b))
            //recipe of depth 3 and 4 children per vertex is already problematic for the enumeration algorithm
        }

        for (b <- Setup.Branching.values; i <- 1 to 100) {
            printRatio(file, recipe(4, 2, b))
            //recipe of depth 4 and 3 children per vertex is already problematic for the enumeration algorithm
        }
    }

    def printRatio(file: Path, recipe: Setup.Recipe): Unit = {
        val faultTree = Setup.makeAlternatingFaultTree(recipe)
        val decisionTree = Setup.translateToDecisionTree(faultTree)

        val heightFaultTree = faulttree.height(faultTree)
        val heightDecisionTree = decisiontree.height.tupled(decisionTree)

        val line = s"\"$faultTree\";$heightDecisionTree;$heightFaultTree;${ratio(heightFaultTree, heightDecisionTree)}\n"
        print(line)
        writeString(file, line)
    }

    def ratio(approximation: Double, realValue: Double): Double = approximation / realValue

    def recipe(depth: Int, width: Int, startNode: Setup.Branching): Setup.Recipe = Setup.Recipe(
        depth = depth,
        branching = Seq(startNode),
        childIsBasicProbability = 0.1,
        branchingWidth = width,
        probabilityOf = id => random()
    )

    @tailrec
    def random(): Double = {
        val r = Math.random()
        if r == 0 then random() else r
    }
}

object Setup {

    enum Branching extends java.lang.Enum[Branching]:
        case And, Or

    def other(branchType: Branching): Branching = (branchType: @switch) match
        case Branching.And => Branching.Or
        case Branching.Or => Branching.And

    type ChildIsBasicProbability = Double
    type BranchingWidth = Int
    type Depth = Int
    type Id = Int
    type Probability = Double
    type ProbabilityGen = Id => Probability

    case class Recipe(depth: Depth,
                      branching: Seq[Branching],
                      childIsBasicProbability: ChildIsBasicProbability,
                      branchingWidth: BranchingWidth,
                      probabilityOf: ProbabilityGen)

    val depth1 = fullTreeRecipe(1)
    val depth2 = fullTreeRecipe(2)
    val depth3 = fullTreeRecipe(3)
    val depth4 = fullTreeRecipe(4)
    val depth5 = fullTreeRecipe(5)

    def fullTreeRecipe(depth: Int): Recipe = Recipe(
        depth = depth,
        branching = Seq(Setup.Branching.And, Setup.Branching.Or),
        childIsBasicProbability = 0,
        branchingWidth = 2,
        probabilityOf = id => 1D / id
    )

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
                    createBalancedAnd(children.map(matchTree), nextId)
                case faulttree.FaultTree.OrEvent(_, children) =>
                    createBalancedOr(children.map(matchTree), nextId)
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
                    _ <- 0 until recipe.branchingWidth
                yield if Math.random() > recipe.childIsBasicProbability then
                    // child is and-gate or or-gate
                    makeFaultTree(recipe.copy(depth = recipe.depth - 1, branching = Seq(other(branchType))))
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

    def makeFaultTree(recipe: Recipe): faulttree.FaultTree = {
        val idGen = new AtomicInteger()

        def makeFaultTree(recipe: Recipe): faulttree.FaultTree = {
            val id = idGen.getAndIncrement()

            if (recipe.depth == 1) {
                faulttree.FaultTree.BasicEvent(id, recipe.probabilityOf(id))
            } else {
                val branchType = recipe.branching(id % recipe.branching.size)
                val children = for
                    _ <- 0 until recipe.branchingWidth
                yield if Math.random() > recipe.childIsBasicProbability then
                    // child is and-gate or or-gate
                    makeFaultTree(recipe.copy(depth = recipe.depth - 1))
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

    def makeDecisionTree(recipe: Recipe): (decisiontree.BooleanFormula, Seq[Probability], (decisiontree.BooleanFormula, Id) => Boolean) = {
        val idGen = new AtomicInteger()
        val probabilitiesBuilder = Seq.newBuilder[Probability]

        def nextId(): Id = {
            val id = idGen.getAndIncrement()
            probabilitiesBuilder.addOne(recipe.probabilityOf(id))
            id
        }

        def makeDecisionTree(recipe: Recipe): decisiontree.BooleanFormula = {
            val id = nextId()

            if recipe.depth == 1 then
                decisiontree.BooleanFormula.Variable(id)
            else
                val branchType = recipe.branching(id % recipe.branching.size)
                val children = for
                    _ <- 0 until recipe.branchingWidth
                yield if Math.random() > recipe.childIsBasicProbability then
                    makeDecisionTree(recipe.copy(depth = recipe.depth - 1)) // and-gate or or-gate
                else
                    decisiontree.BooleanFormula.Variable(nextId()) // basic event

                branchType match
                    case Branching.And => createBalancedAnd(children, nextId)
                    case Branching.Or => createBalancedOr(children, nextId)
            end if
        }

        val tree = makeDecisionTree(recipe)
        val probabilities = probabilitiesBuilder.result()
        val variableLookup: (decisiontree.BooleanFormula, Id) => Boolean = decisiontree.computeLookupBiId()
        (tree, probabilities, variableLookup)
    }

    private def createBalancedOr(children: Seq[decisiontree.BooleanFormula], nextId: () => Id): decisiontree.BooleanFormula = children match {
        case Seq(single) => single
        case _ =>
            val nodeCount = children.size

            val leftHalf = nodeCount / 2
            val rightHalf = nodeCount - leftHalf

            val (leftChildren, rightChildren) = children.splitAt(leftHalf)
            decisiontree.BooleanFormula.Or(createBalancedOr(leftChildren, nextId), createBalancedOr(rightChildren, nextId))
    }

    private def createBalancedAnd(children: Seq[decisiontree.BooleanFormula], nextId: () => Id): decisiontree.BooleanFormula = children match {
        case Seq(single) => single
        case _ =>
            val nodeCount = children.size

            val leftHalf = nodeCount / 2
            val rightHalf = nodeCount - leftHalf

            val (leftChildren, rightChildren) = children.splitAt(leftHalf)
            decisiontree.BooleanFormula.And(createBalancedAnd(leftChildren, nextId), createBalancedAnd(rightChildren, nextId))
    }

    // main method to manually verify our generators indeed work the same way - the generated trees are 'the same'.
    def main(args: Array[String]): Unit = {
        //val recipe = depth4;
        val recipe = Recipe(
            depth = 4,
            branching = Seq(Branching.And, Branching.Or),
            childIsBasicProbability = 0,
            branchingWidth = 3,
            probabilityOf = id => 1D / id
        )

        val (decisionTree, probabilities, containsId) = makeDecisionTree(recipe)
        println(ppDecisionTree(decisionTree, probabilities))

        val faultTree = makeFaultTree(recipe)
        val layers = faulttree.layers(faultTree)
        println(ppFaultTree(faultTree))

        // calculate heights, they should be the same also.
//        println(decisiontree.height(decisionTree, probabilities, containsId))
//        println(faulttree.height(faultTree, layers))

        val randomTree = makeAlternatingFaultTree(Recipe(
            depth = 3,
            branching = Seq(Branching.And),
            childIsBasicProbability = 0,
            branchingWidth = 3,
            probabilityOf = id => Math.random()
        ))

        println(ppFaultTree(randomTree))
        println(ppDecisionTree.tupled(translateToDecisionTree(randomTree)))
    }

    def ppDecisionTree(tree: decisiontree.BooleanFormula, probabilities: Seq[Probability]): String = tree match {
        case decisiontree.BooleanFormula.Variable(id) => s"Leaf(id=${id},prob=${probabilities(id)})"
        case decisiontree.BooleanFormula.Or(left, right) => s"Or(${ppDecisionTree(left, probabilities)},${ppDecisionTree(right, probabilities)})"
        case decisiontree.BooleanFormula.And(left, right) => s"And(${ppDecisionTree(left, probabilities)},${ppDecisionTree(right, probabilities)})"
    }

    def ppFaultTree(tree: faulttree.FaultTree): String = tree match {
        case faulttree.FaultTree.BasicEvent(id, prob) => s"Leaf(id=${id},prob=${prob})"
        case faulttree.FaultTree.OrEvent(id, children) => s"Or(${children.map(ppFaultTree).mkString(",")})"
        case faulttree.FaultTree.AndEvent(id, children) => s"And(${children.map(ppFaultTree).mkString(",")})"
    }
}