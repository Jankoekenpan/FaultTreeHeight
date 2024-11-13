package benchmark

import scala.collection.immutable.IntMap

object Conversion {

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

    // TODO this seems to have a bug...
    def translateToTreeLikeFaultTree(dagFT: minimalcutpathset.FaultTree): faulttree.FaultTree = {
        def recur(node: minimalcutpathset.TreeNode): faulttree.FaultTree = {
            node match {
                case minimalcutpathset.TreeNode.BasicEvent(id, probability) => faulttree.FaultTree.BasicEvent(id, probability)
                case minimalcutpathset.TreeNode.Combination(id, minimalcutpathset.Gate.And, children) =>
                    faulttree.FaultTree.AndEvent(id, children.toSeq.map(c => recur(dagFT.node(c))))
                case minimalcutpathset.TreeNode.Combination(id, minimalcutpathset.Gate.Or, children) =>
                    faulttree.FaultTree.OrEvent(id, children.toSeq.map(c => recur(dagFT.node(c))))
            }
        }

        recur(dagFT.topNode)
    }

    def translateToDecisionTree(dagFT: minimalcutpathset.FaultTree): (decisiontree.BooleanFormula, Seq[Double]) =
        translateToDecisionTree(translateToTreeLikeFaultTree(dagFT))

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

    def main(args: Array[String]): Unit = {
        import minimalcutpathset.{FaultTree, TreeNode, Gate}
        import decisiontree.BooleanFormula.*

        val dagTree = FaultTree(8, Map(
            8 -> TreeNode.Combination(8, Gate.And, Set(3, 0, 2, 6, 7)),
            7 -> TreeNode.Combination(7, Gate.And, Set(0, 2)),
            6 -> TreeNode.Combination(6, Gate.And, Set(3, 0)),
            //5 -> TreeNode.Combination(5, Gate.Or, Set(2)),
            //4 -> TreeNode.Combination(4, Gate.And, Set(0)),
            3 -> TreeNode.Combination(3, Gate.Or, Set(1, 2)),
            2 -> TreeNode.BasicEvent(2, 0.3),
            1 -> TreeNode.BasicEvent(1, 0.2),
            0 -> TreeNode.BasicEvent(0, 0.1)
        ))

        val treeLikeFaultTree = translateToTreeLikeFaultTree(dagTree)
        println(treeLikeFaultTree)
        println()

        val (formula, probabilities) = translateToDecisionTree(dagTree)
        println(formula)
        println(probabilities)
        println()

        val (newFormula, newProbabilities) = (
            And(Or(Variable(0), Variable(1)), And(Variable(0), Variable(2))),
            Seq(0.1, 0.2, 0.3)
        )

        val h1 = decisiontree.height(formula, probabilities)
        val h2 = faulttree.height(treeLikeFaultTree)
        val h3 = minimalcutpathset.height4(dagTree)
        val h4 = minimalcutpathset.height5(dagTree)
        val h5 = decisiontree.height(newFormula, newProbabilities)

        println(h1)
        println(h2)
        println(h3) // SHOULDN'T BE LOWER THAN h1!
        println(h4) // SHOULDN'T BE LOWER THAN h1!
        println(h5)
    }
}
