package benchmark

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, Setup, State}

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import scala.compiletime.uninitialized

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class MyBenchmark {

    private var decisionTree10: decisiontree.BooleanFormula = uninitialized
    private var decisionTree10Probabilities: Seq[Double] = uninitialized
    private var decisionTree10IdLookup: (decisiontree.BooleanFormula, Int) => Boolean = uninitialized

    private var faultTree10: faulttree.FaultTree = uninitialized
    private var faultTree10Layers: IArray[Seq[faulttree.FaultTree]] = uninitialized

    private var exampleDecisionTree: decisiontree.BooleanFormula = uninitialized
    private var exampleDecisionTreeProbabilities: Seq[Double] = uninitialized
    private var exampleDecisionTreeLookup: (decisiontree.BooleanFormula, Int) => Boolean = uninitialized

    private var exampleFaultTree: faulttree.FaultTree = uninitialized
    private var exampleFaultTreeLayers: IArray[Seq[faulttree.FaultTree]] = uninitialized

    @Setup
    def setup(): Unit = {
        val (decisionTree10, decisionTree10Probabilities, decisionTree10IdChecker) = Setup.makeDecisionTree(Setup.depth10)
        this.decisionTree10 = decisionTree10
        this.decisionTree10Probabilities = decisionTree10Probabilities
        this.decisionTree10IdLookup = decisionTree10IdChecker

        this.faultTree10 = Setup.makeFaultTree(Setup.depth10)
        this.faultTree10Layers = faulttree.layers(faultTree10)

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
    def testHeightDecisionTree10(): Double = {
        decisiontree.height(decisionTree10, decisionTree10Probabilities, decisionTree10IdLookup)
    }

    @Benchmark
    def testHeightFaultTree10(): Double = {
        faulttree.height(faultTree10, faultTree10Layers)
    }
}

object Setup {

    enum Branching extends java.lang.Enum[Branching]:
        case And, Or

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

    val depth10 = Recipe(
        depth = 10,
        branching = Seq(Setup.Branching.And, Setup.Branching.Or),
        childIsBasicProbability = 0,
        branchingWidth = 2,
        probabilityOf = id => 1D / id
    )

    val depth100 = Recipe(
        depth = 100,
        branching = Seq(Setup.Branching.And, Setup.Branching.Or),
        childIsBasicProbability = 0,
        branchingWidth = 2,
        probabilityOf = id => 1D / id
    )

    val depth1000 = Recipe(
        depth = 1000,
        branching = Seq(Setup.Branching.And, Setup.Branching.Or),
        childIsBasicProbability = 0,
        branchingWidth = 2,
        probabilityOf = id => 1D / id
    )

    val depth10000 = Recipe(
        depth = 10000,
        branching = Seq(Setup.Branching.And, Setup.Branching.Or),
        childIsBasicProbability = 0,
        branchingWidth = 2,
        probabilityOf = id => 1D / id
    )

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
            decisiontree.BooleanFormula.And(createBalancedOr(leftChildren, nextId), createBalancedOr(rightChildren, nextId))
    }

    // main method to manually verify our generators indeed work the same way - the generated trees are 'the same'.
    @main
    def main(): Unit = {
        val (decisionTree10, probabilities10, containsId10) = makeDecisionTree(depth10)
        println(ppDecisionTree(decisionTree10, probabilities10))

        val faultTree10 = makeFaultTree(depth10)
        val layers10 = faulttree.layers(faultTree10)
        println(ppFaultTree(faultTree10))

        // calculate heights, they should be the same also.
        println(decisiontree.height(decisionTree10, probabilities10, containsId10))
        println(faulttree.height(faultTree10, layers10))
    }

    def ppDecisionTree(tree: decisiontree.BooleanFormula, probabilities: Seq[Probability]): String = tree match {
        case decisiontree.BooleanFormula.Variable(id) => s"id=${id},prob=${probabilities(id)}"
        case decisiontree.BooleanFormula.Or(left, right) => s"Or(${ppDecisionTree(left, probabilities)},${ppDecisionTree(right, probabilities)})"
        case decisiontree.BooleanFormula.And(left, right) => s"And(${ppDecisionTree(left, probabilities)},${ppDecisionTree(right, probabilities)})"
    }

    def ppFaultTree(tree: faulttree.FaultTree): String = tree match {
        case faulttree.FaultTree.BasicEvent(id, prob) => s"id=${id},prob=${prob}"
        case faulttree.FaultTree.OrEvent(id, children) => s"Or(${children.map(ppFaultTree).mkString(",")})"
        case faulttree.FaultTree.AndEvent(id, children) => s"And(${children.map(ppFaultTree).mkString(",")})"
    }
}