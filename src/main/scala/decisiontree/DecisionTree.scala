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

def normalise(formula: BooleanFormula): BooleanFormula = formula match
    // Distributive Laws
    case Or(And(a, b), c) =>
        normalise(And(Or(a, c), Or(b, c)))
    case Or(a, And(b, c)) =>
        normalise(And(Or(a, b), Or(a, c)))

    // Identity Laws
    case And(a, True) => normalise(a)
    case And(True, b) => normalise(b)
    case Or(a, False) => normalise(a)
    case Or(False, b) => normalise(b)

    // Domination Laws
    case And(_, False) => False
    case And(False, _) => False
    case Or(_, True) => True
    case Or(True, _) => True

    // Idempotent Laws
    case And(a, b) if a == b => normalise(a)
    case Or(a, b) if a == b => normalise(a)

    // Complement Laws
    case And(a, Or(b, _)) if a == b => False
    case Or(a, And(b, _)) if a == b => True

    // Recursively normalize
    case And(a, b) => 
        val normalizedLeft = normalise(a)
        val normalizedRight = normalise(b)
        if normalizedLeft == normalizedRight then normalizedLeft else And(normalizedLeft, normalizedRight)
    
    case Or(a, b) => 
        val normalizedLeft = normalise(a)
        val normalizedRight = normalise(b)
        if normalizedLeft == normalizedRight then normalizedLeft else Or(normalizedLeft, normalizedRight)

    // Base cases
    case _ => formula

def literal(boolean: Boolean): BooleanFormula = boolean match
    case true => True
    case false => False
    
def substitute(formula/*already normalised*/: BooleanFormula, variableId: Int, value: Boolean): BooleanFormula = formula match
    case v @ Variable(id) => if id == variableId then literal(value) else v
    case Or(left, right) => Or(substitute(left, variableId, value), substitute(right, variableId, value))
    case And(left, right) => And(substitute(left, variableId, value), substitute(right, variableId, value))
    case _ => formula

type RealNumber = Double    // We use IEEE 754 double precision floating point numbers for speed. Can change it to BigDecimal at any time for better accuracy.

def subsuper(formula: BooleanFormula, superscript: Id, subscript: Boolean) =
    normalise(substitute(formula, superscript, subscript))

// TODO can we memoize the result of a height calculation? which one do we cache? we should cache heights for at least (k, formula) pairs, maybe also just heights of (formula)s.
// TODO we should probably analyse why we run out of java heap (use Eclipse MAT and dump heap on oom jvm flag)

def height(formula: BooleanFormula, probabilities: Seq[RealNumber], containsVariable: (BooleanFormula, Id) => Boolean): RealNumber = formula match
    case BooleanFormula.True => 0   // used to be 1.
    case BooleanFormula.False => 0  // used to be 1.
    case _ =>
        var min = Double.MaxValue
        for k <- probabilities.indices do
            if containsVariable(formula, k) then
                min = Math.min(min, height(k, formula, probabilities, containsVariable))
        min

def height(k/*zero-based*/: Int, formula: BooleanFormula, probabilities: Seq[RealNumber], containsVariable: (BooleanFormula, Id) => Boolean): RealNumber =
    val pk = probabilities(k)
    val fk1 = subsuper(formula, k, true)
    val fk0 = subsuper(formula, k, false)
    1 + pk * height(fk1, probabilities, containsVariable) + (1 - pk) * height(fk0, probabilities, containsVariable)

def height(formula: BooleanFormula, probabilities: Seq[RealNumber]): RealNumber =
    height(formula, probabilities, computeLookupBiId())

@main def main(): Unit = {
//    val formula = And(Or(Variable(0), Variable(1)), Variable(2))
//    val probabilities = Seq(1D/2D, 1D/3D, 1D/4D)

    val formula = And(
        Or(Variable(0), Variable(1)),
        Or(Variable(2), Variable(3))
    )
    val probabilities = Seq(1D/2D, 1D/3D, 1D/4D, 1D/5D)

    println(height(formula, probabilities, computeLookupBiId()))
}