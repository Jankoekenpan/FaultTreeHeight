package decisiontree

type Id = Int

enum BooleanFormula:
    case And(left: BooleanFormula, right: BooleanFormula)
    case Or(left: BooleanFormula, right: BooleanFormula)
    case Variable(id: Id)
    case True
    case False

import BooleanFormula.*

def computeLookupBiId()(lookup: BooleanFormula, variableId: Id): Boolean = {
    val lookupMap = new java.util.WeakHashMap[BooleanFormula, Set[Id]]()

    def variables(formula: BooleanFormula): Set[Id] = {
        var variablesInFormula = lookupMap.get(formula)

        if variablesInFormula == null then
            variablesInFormula = formula match
                case And(lhs, rhs) => variables(lhs) union variables(rhs)
                case Or(lhs, rhs) => variables(lhs) union variables(rhs)
                case Variable(id) => Set(id)
                case True | False => Set()
            lookupMap.put(formula, variablesInFormula)

        variablesInFormula
    }

    variables(lookup).contains(variableId)
}

def subsuper(formula: BooleanFormula, superscript: Id, subscript: Boolean): BooleanFormula = formula match {
    case lit @ (True | False) => lit
    case v @ Variable(id) => if id == superscript then literal(subscript) else v
    case And(_, False) => False
    case And(False, _) => False
    case Or(True, _) => True
    case Or(_, True) => True
    case And(True, b) => subsuper(b, superscript, subscript)
    case And(a, True) => subsuper(a, superscript, subscript)
    case Or(False, b) => subsuper(b, superscript, subscript)
    case Or(a, False) => subsuper(a, superscript, subscript)
    case conjunction @ And(lhs, rhs) =>
        val nlhs = subsuper(lhs, superscript, subscript)
        val nrhs = subsuper(rhs, superscript, subscript)
        if nlhs == lhs && nrhs == rhs then conjunction else subsuper(And(nlhs, nrhs), superscript, subscript)
    case disjunction @ Or(lhs, rhs) =>
        val nlhs = subsuper(lhs, superscript, subscript)
        val nrhs = subsuper(rhs, superscript, subscript)
        if nlhs == lhs && nrhs == rhs then disjunction else subsuper(Or(nlhs, nrhs), superscript, subscript)
}

def literal(boolean: Boolean): BooleanFormula = boolean match
    case true => True
    case false => False

type RealNumber = Double    // We use IEEE 754 double precision floating point numbers for speed. Can change it to BigDecimal at any time for better accuracy.

// TODO could we do memoisation of some heights?
// TODO could probably use a WeakHashMap.

def height(formula: BooleanFormula, probabilities: Seq[RealNumber], containsVariable: (BooleanFormula, Id) => Boolean): RealNumber = formula match
    case BooleanFormula.True => 0   // used to be 1.
    case BooleanFormula.False => 0  // used to be 1.
    case _ =>
        val heights = for
            k <- probabilities.indices
            if containsVariable(formula, k)
        yield height(k, formula, probabilities, containsVariable)
        heights.min

def height(k/*zero-based*/: Int, formula: BooleanFormula, probabilities: Seq[RealNumber], containsVariable: (BooleanFormula, Id) => Boolean): RealNumber =
    val pk = probabilities(k)
    val fk1 = subsuper(formula, k, true)
    val fk0 = subsuper(formula, k, false)
    1 + pk * height(fk1, probabilities, containsVariable) + (1 - pk) * height(fk0, probabilities, containsVariable)

def height(formula: BooleanFormula, probabilities: Seq[RealNumber]): RealNumber =
    height(formula, probabilities, computeLookupBiId())

@main def main(): Unit = {
    // 1.375
//    val formula = And(Or(Variable(0), Variable(1)), Variable(2))
//    val probabilities = Seq(1D/2D, 1D/3D, 1D/4D)

    // 2.35
//    val formula = And(
//        Or(Variable(0), Variable(1)),
//        Or(Variable(2), Variable(3))
//    )
//    val probabilities = Seq(1D/2D, 1D/3D, 1D/4D, 1D/5D)

    // 1.08
    val formula = problematicTree
    val probabilities = Seq(1D/3D, 1D/4D, 1D/6D, 1D/7D, 1D/10D, 1D/11D, 1D/13D, 1D/14D)

    println(height(formula, probabilities, computeLookupBiId()))
}

val problematicTree: BooleanFormula = And(
    Or(
        And(
            Variable(0),
            Variable(1)
        ),
        Or(
            Variable(2),
            Variable(3)
        )
    ),
    And(
        Or(
            Variable(4),
            Variable(5)
        ),
        And(
            Variable(6),
            Variable(7)
        )
    )
)