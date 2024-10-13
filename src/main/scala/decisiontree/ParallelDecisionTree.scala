package decisiontree

import decisiontree.{BooleanFormula, Id, RealNumber, computeLookupBiId, subsuper}
import decisiontree.BooleanFormula.*
import util.ConcurrentReferenceHashMap

import java.util.OptionalDouble
import java.util.concurrent.{Callable, StructuredTaskScope}

import scala.util.Using

class CCache(heights: ConcurrentReferenceHashMap[BooleanFormula, RealNumber],
            heightKs: ConcurrentReferenceHashMap[(BooleanFormula, Int), RealNumber]) {

    def this() = {
        this(new ConcurrentReferenceHashMap(), new ConcurrentReferenceHashMap())
    }

    def getHeight(booleanFormula: BooleanFormula): OptionalDouble = {
        val heightValue: java.lang.Double = heights.get(booleanFormula)
        if heightValue == null then OptionalDouble.empty() else OptionalDouble.of(heightValue)
    }

    def putHeight(booleanFormula: BooleanFormula, heightValue: RealNumber): Unit = {
        heights.put(booleanFormula, heightValue)
    }

    def getHeightK(booleanFormula: BooleanFormula, k: Int): OptionalDouble = {
        val heightKValue: java.lang.Double = heightKs.get((booleanFormula, k))
        if heightKValue == null then OptionalDouble.empty() else OptionalDouble.of(heightKValue)
    }

    def putHeightK(booleanFormula: BooleanFormula, k: Int, heightKValue: RealNumber): Unit = {
        heightKs.put((booleanFormula, k), heightKValue)
    }
}

def parHeight(formula: BooleanFormula, probabilities: Seq[RealNumber], containsVariable: (BooleanFormula, Id) => Boolean, cache: CCache): RealNumber = {
    val cachedHeight = cache.getHeight(formula)
    if cachedHeight.isPresent then return cachedHeight.getAsDouble

    val result = formula match
        case BooleanFormula.True => 0 // used to be 1.
        case BooleanFormula.False => 0 // used to be 1.
        case _ =>
            val scope = new StructuredTaskScope[RealNumber]()
            val heightComputations: Seq[StructuredTaskScope.Subtask[RealNumber]] = for
                k <- probabilities.indices
                if containsVariable(formula, k)
            yield scope.fork(() => parHeightK(k, formula, probabilities, containsVariable, cache))
            scope.join()
            heightComputations.map(_.get()).min
    cache.putHeight(formula, result)
    result
}

def parHeightK(k/*zero-based*/: Int, formula: BooleanFormula, probabilities: Seq[RealNumber], containsVariable: (BooleanFormula, Id) => Boolean, cache: CCache): RealNumber = {
    val cachedHeightK = cache.getHeightK(formula, k)
    if cachedHeightK.isPresent then return cachedHeightK.getAsDouble

    val pk = probabilities(k)
    val fk1 = subsuper(formula, k, true)
    val fk0 = subsuper(formula, k, false)

    val heightFk1Computation: Callable[RealNumber] = () => parHeight(fk1, probabilities, containsVariable, cache)
    val heightFk0Computation: Callable[RealNumber] = () => parHeight(fk0, probabilities, containsVariable, cache)

    val result: RealNumber = Using(new StructuredTaskScope[RealNumber]()) { scope =>
        val task1 = scope.fork(heightFk1Computation)
        val task0 = scope.fork(heightFk0Computation)

        scope.join()

        1 + pk * task1.get() + (1 - pk) * task0.get()
    }.get

    cache.putHeightK(formula, k, result)
    result
}

def parHeight(formula: BooleanFormula, probabilities: Seq[RealNumber]): RealNumber =
    parHeight(formula, probabilities, computeLookupBiId(), CCache())

@main def parMain(): Unit = {

    val (tree, probabilities) = treeAndProbabilities
    println(parHeight(tree, probabilities))


}

import faulttree.FaultTree.{BasicEvent => Basic, AndEvent => And, OrEvent => Or}

val faultTree: faulttree.FaultTree = And(100, Seq(
    Or(101, Seq(
        Basic(1, 1D/2D),
        Basic(13, 1D/2D),
        And(102, Seq(
            Basic(3, 1D/2D),
            Basic(4, 1D/2D),
            Or(103, Seq(
                Basic(11, 1D/2D),
                Basic(12, 1D/2D),
                Basic(15, 1D/2D)
            )),
            Basic(5, 1D/2D),
            Basic(14, 1D/2D)
        )),
        And(103, Seq(
            Basic(6, 1D/2D),
            Basic(7, 1D/2D),
            Basic(16, 1D/2D),
        ))
    )),
    Or(104, Seq(
        And(105, Seq(
            Basic(8, 1D/2D),
            Basic(9, 1D/2D),
            Basic(10, 1D/2D),
//            Basic(18, 1D/2D)
        )),
        Basic(2, 1D/2D),
        Basic(17, 1D/2D)
    ))
))

val treeAndProbabilities: (BooleanFormula, Seq[RealNumber]) = (
    BooleanFormula.And(
        BooleanFormula.Or(
            BooleanFormula.And(
                BooleanFormula.Variable(0),
                BooleanFormula.And(
                    BooleanFormula.Variable(1),
                    BooleanFormula.Variable(2)
                )
            ),
            BooleanFormula.And(
                BooleanFormula.Variable(3),
                BooleanFormula.Variable(4)
            )
        ),
        BooleanFormula.And(
            BooleanFormula.Or(
                BooleanFormula.And(
                    BooleanFormula.Variable(5),
                    BooleanFormula.Variable(6)
                ),
                BooleanFormula.Or(
                    BooleanFormula.And(
                        BooleanFormula.Variable(7),
                        BooleanFormula.Variable(8)
                    ),
                    BooleanFormula.And(
                        BooleanFormula.Variable(9),
                        BooleanFormula.And(
                            BooleanFormula.Variable(10),
                            BooleanFormula.Variable(11)
                        )
                    )
                )
            ),
            BooleanFormula.Or(
                BooleanFormula.And(
                    BooleanFormula.Variable(12),
                    BooleanFormula.And(
                        BooleanFormula.Variable(13),
                        BooleanFormula.Variable(14)
                    )
                ),
                BooleanFormula.Or(
                    BooleanFormula.And(
                        BooleanFormula.Variable(15),
                        BooleanFormula.And(
                            BooleanFormula.Variable(16),
                            BooleanFormula.Variable(17)
                        )
                    ),
                    BooleanFormula.And(
                        BooleanFormula.Variable(18),
                        BooleanFormula.And(
                            BooleanFormula.Variable(19),
                            BooleanFormula.Variable(20)
                        )
                    )
                )
            )
        )
    ),
    List(0.06172645973693747, 0.6824415574226003, 0.4773507774046718, 0.1271350784655565, 0.22878253470882592, 0.43147589332099523, 0.1864100842108677, 0.14924661841572395, 0.26146051193359743, 0.17275744429486384, 0.6793904018857394, 0.07601495143767734, 0.46259104870109613, 0.23735070936925473, 0.7197135209516127, 0.8750143322971985, 0.38785116707127154, 0.2379363221362475, 0.9678709126458386, 0.5253740653156019, 0.947078704870316)
)