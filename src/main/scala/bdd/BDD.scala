package bdd

import scala.collection.immutable.IntMap

/*
Explanation from Matthias:

The BDDs are represented internally in Sylvan as BDDs with complement
edges. Each edges which has a black dot at the source of the edge
indicates such a complement edge. Complement edges negate the
corresponding value. So if you have a complement edge leading to false
it actually represents true. Sylvan uses complement edges for a more
compact representation.
The BDDs should therefore be correct, they are just visualized differently.
 */

enum BDD:
    case True
    case False
    case Node(id: Int, trueBranch: BDD, falseBranch: BDD)

object BDD {

    def height(bdd: BDD, probabilities: IntMap[Double]): Double = bdd match {
        case BDD.True => 0
        case BDD.False => 0
        case BDD.Node(id, trueBranch, falseBranch) =>
            val pk = probabilities(id)
            // TODO check with Yanni: is this calculation correct?
            1 + pk * height(trueBranch, probabilities) + (1 - pk) * height(falseBranch, probabilities)
    }

}